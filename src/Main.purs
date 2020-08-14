module Main where

import Prelude

import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

data Action
  = TextFired String

data Filtering
  = All
  | Active
  | Completed

derive instance eqFiltering :: Eq Filtering
derive instance genericFiltering :: Generic Filtering _
instance showFiltering :: Show Filtering where
  show = genericShow

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
          $ map renderRow state.rows
        ]
      , HH.footer [ HP.class_ $ ClassName "footer" ]
        [ HH.span [ HP.class_ $ ClassName "todo-count" ]
          [ HH.text "items left" ]
        , HH.ul [ HP.class_ $ ClassName "filters" ]
          $ flip map [All, Active, Completed] \f ->
            HH.li_ [ HH.a (if state.filtering == f then [ HP.class_ $ ClassName "selected" ] else []) [ HH.text $ show f ] ]
        , HH.button [ HP.class_ $ ClassName "clear-completed" ]
          [ HH.text "Clear completed (1)" ]
        ]
      ]
    , HH.footer [ HP.class_ $ ClassName "info" ]
      [ HH.p_ [ HH.text "Double click to edit a todo" ]
      ]
    ]

  renderRow r =
    HH.li (if r.done then [ HP.class_ $ ClassName "completed" ] else [])
    [ HH.div [ HP.class_ $ ClassName "view" ]
      [ HH.input
        [ HP.class_ $ ClassName "toggle"
        , HP.type_ HP.InputCheckbox
        ]
      , HH.label_ [ HH.text r.text ]
      , HH.button [ HP.class_ $ ClassName "destroy" ]
        []
      ]
    , HH.input [ HP.class_ $ ClassName "edit" ]
    ]

  handleAction = case _ of
    TextFired v -> do
      H.modify_ \state -> state { text = v }

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI component unit body
