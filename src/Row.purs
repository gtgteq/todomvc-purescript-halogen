module Row (Slot, Query(..), Message(..), State, component) where

import Prelude

import Data.Maybe (Maybe(..))
import Data.String as S
import Effect.Class (class MonadEffect)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Core (ClassName(..))
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Web.Event.Event (Event, preventDefault)

type Slot = H.Slot Query Message

data Query a
  = IsDone (Boolean -> a)
  | IsEditing (Boolean -> a)

data Message
  = Changed State
  | Destroy

data Action
  = ToggleDone
  | ToggleEditing
  | TriggerDestroy
  | ChangeText String
  | EnterText

type State =
  { done :: Boolean
  , editing :: Boolean
  , text :: String
  }

type InternalState =
  { done :: Boolean
  , editing :: Boolean
  , previous :: String
  , text :: String
  }

component :: forall m. MonadEffect m => H.Component HH.HTML Query State Message m
component =
  H.mkComponent
    { initialState: \{ done, editing, text } -> { done, editing, text, previous: text }
    , render
    , eval: H.mkEval $ H.defaultEval
      { handleAction = handleAction
      , handleQuery = handleQuery
      }
    }

toExternal :: InternalState -> State
toExternal { done, editing, text } = { done, editing, text }

render :: forall m. InternalState -> H.ComponentHTML Action () m
render r =
  HH.li
  [ HP.classes
    $ (if r.done then [ ClassName "completed" ] else [])
    <> (if r.editing then [ ClassName "editing" ] else [])
  ]
  [ HH.div [ HP.class_ $ ClassName "view" ]
    [ HH.input
      [ HP.class_ $ ClassName "toggle"
      , HP.type_ HP.InputCheckbox
      , HP.checked r.done
      , HE.onClick \_ -> Just ToggleDone
      ]
    , HH.label
      [ HE.onDoubleClick \_ -> Just ToggleEditing ]
      [ HH.text r.text ]
    , HH.button
      [ HP.class_ $ ClassName "destroy"
      , HE.onClick \_ -> Just TriggerDestroy
      ]
      []
    ]
  , HH.input
    [ HE.onValueInput $ Just <<< ChangeText
    , HE.onValueChange \_ -> Just EnterText
    , HP.class_ $ ClassName "edit"
    , HP.value r.text
    ]
  ]

handleAction :: forall m. MonadEffect m => Action -> H.HalogenM InternalState Action () Message m Unit
handleAction = case _ of
  ToggleDone -> do
    newRow <- H.modify \r -> r { done = not r.done }
    H.raise $ Changed $ toExternal newRow
  ToggleEditing -> do
    newRow <- H.modify \r -> r { editing = not r.editing }
    H.raise $ Changed $ toExternal newRow
  TriggerDestroy -> do
    H.raise Destroy
  ChangeText v -> do
    H.modify_ \r -> r { text = v }
  EnterText -> do
    newRow <- H.modify \r -> r
      { editing = not r.editing
      , previous = if S.null r.text then r.previous else r.text
      , text = if S.null r.text then r.previous else r.text
      }
    H.raise $ Changed $ toExternal newRow

handleQuery :: forall m a. Query a -> H.HalogenM InternalState Action () Message m (Maybe a)
handleQuery = case _ of
  IsDone f -> do
    done <- H.gets _.done
    pure $ Just $ f done
  IsEditing f -> do
    editing <- H.gets _.editing
    pure $ Just $ f editing
