port module Main exposing (..)

-- A text input for reversing text. Very useful!
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/text_fields.html
--

import Browser
import Html exposing (Html, Attribute, div, input, text, textarea, pre)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput)

import Phi

port codeUpdateReceiver : (String -> msg) -> Sub msg

-- MAIN


main =
  Browser.element
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }

-- MODEL


type alias Model =
  { content : String
  }


init : () -> ( Model, Cmd Msg )
init flags =
  ( { content = "" }
  , Cmd.none
  )



-- UPDATE


type Msg
  = Change String

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  case msg of
    Change newContent ->
      ( { model | content = Phi.interpret newContent }
      , Cmd.none
      )


-- SUBSCRIPTIONS


-- Subscribe to the `messageReceiver` port to hear about messages coming in
-- from JS. Check out the index.html file to see how this is hooked up to a
-- WebSocket.
--
subscriptions : Model -> Sub Msg
subscriptions _ =
  codeUpdateReceiver Change


-- VIEW


view : Model -> Html Msg
view model =
  div []
    [ input
      [ placeholder "Text to reverse"
      , value model.content
      , onInput Change
      , hidden True
      ] []
    , div [] [ pre [] [ text model.content ] ]
    ]
