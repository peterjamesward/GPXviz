module Main exposing (main)

import Browser
import File exposing (File)
import File.Select as Select
import Html exposing (Html, button, p, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Task



-- MAIN


main : Program () Model Msg
main =
  Browser.element
    { init = init
    , view = view
    , update = update
    , subscriptions = subscriptions
    }



-- MODEL


type alias Model =
  { gpx : Maybe String
  }


init : () -> (Model, Cmd Msg)
init _ =
  ( Model Nothing, Cmd.none )



-- UPDATE


type Msg
  = GpxRequested
  | GpxSelected File
  | GpxLoaded String


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GpxRequested ->
      ( model
      , Select.file ["text/gpx"] GpxSelected
      )

    GpxSelected file ->
      ( model
      , Task.perform GpxLoaded (File.toString file)
      )

    GpxLoaded content ->
      ( { model | gpx = Just content }
      , Cmd.none
      )



-- VIEW


view : Model -> Html Msg
view model =
  case model.gpx of
    Nothing ->
      button [ onClick GpxRequested ] [ text "Load GPX" ]

    Just content ->
      p [ style "white-space" "pre" ] [ text content ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
  Sub.none
