module Main where

import Prelude

import Data.Array (catMaybes)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)
import Web.Event.Event (Event, preventDefault)
import Web.UIEvent.MouseEvent (toEvent)

import Row as Row

data Action
  = TextFired String
  | HandleRow Int Row.Message
  | ChangeFiltering Filtering Event

data Filtering
  = All
  | Active
  | Completed

derive instance eqFiltering :: Eq Filtering
derive instance genericFiltering :: Generic Filtering _
instance showFiltering :: Show Filtering where
  show = genericShow

filterRow All {} = true
filterRow Active { done } = not done
filterRow Completed { done } = done

_row :: SProxy "row"
_row = SProxy

component :: forall f i o m. MonadAff m => H.Component HH.HTML f i o m
component =
  H.mkComponent
    { initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
    }
  where
  initialState _ =
    { text: ""
    , rows:
      [ { done: true, editing: false, text: "one" }
      , { done: false, editing: false, text: "two" }
      ]
    , filtering: All
    }

  render state =
    HH.div_
    [ HH.section [ HP.class_ $ ClassName "todoapp" ]
      [ HH.header [ HP.class_ $ ClassName "header" ]
        [ HH.h1_ [ HH.text "todos" ]
        , HH.input
          [ HE.onValueInput $ Just <<< TextFired
          , HP.class_ $ ClassName "new-todo"
          , HP.placeholder "What needs to be done?"
          ]
        ]
      , HH.section [ HP.class_ $ ClassName "main" ]
        [ HH.input
          [ HP.class_ $ ClassName "toggle-all"
          , HP.type_ HP.InputCheckbox
          ]
        , HH.label [ HP.for "toggle-all" ] [ HH.text "Mark all as complete" ]
        , HH.ul [ HP.class_ $ ClassName "todo-list" ]
          $ catMaybes $ flip mapWithIndex state.rows \i r ->
            if filterRow state.filtering r
            then Just $ HH.slot _row i Row.component r (Just <<< HandleRow i)
            else Nothing
        ]
      , HH.footer [ HP.class_ $ ClassName "footer" ]
        [ HH.span [ HP.class_ $ ClassName "todo-count" ]
          [ HH.text "items left" ]
        , HH.ul [ HP.class_ $ ClassName "filters" ]
          $ flip map [All, Active, Completed] \f ->
            HH.li_
            [ HH.a
              [ HE.onClick $ Just <<< ChangeFiltering f <<< toEvent
              , HP.classes $ if state.filtering == f then [ ClassName "selected" ] else []
              ] [ HH.text $ show f ] ]
        , HH.button [ HP.class_ $ ClassName "clear-completed" ]
          [ HH.text "Clear completed (1)" ]
        ]
      ]
    , HH.footer [ HP.class_ $ ClassName "info" ]
      [ HH.p_ [ HH.text "Double click to edit a todo" ]
      ]
    ]

  handleAction = case _ of
    TextFired v -> do
      H.modify_ \state -> state { text = v }
    HandleRow i v -> pure unit
    ChangeFiltering f event -> do
      H.liftEffect $ preventDefault event
      H.modify_ \state -> state { filtering = f }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
