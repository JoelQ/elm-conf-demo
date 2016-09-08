module Main exposing (..)

import Html.App
import Html exposing (..)


type alias Model =
    ()


init : ( Model, Cmd Msg )
init =
    ( (), Cmd.none )



-- VIEW


view : Model -> Html a
view model =
    main' []
        [ h1 [] [ text "Elm Conf" ]
        ]


type Msg
    = Noop


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
