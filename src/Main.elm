module Main exposing (..)

import Html.App
import Html exposing (..)
import Html.Attributes exposing (style)
import String
import Roman exposing (Roman, Name(..), Children(..), Id, caesar, cornelia)


type alias Model =
    List Roman


        name =
            FemaleName (Just "Africana")
    in
        Roman clan name


init : ( Model, Cmd Msg )
init =
    ( [ caesar, cornelia ], Cmd.none )



-- VIEW


view : Model -> Html a
view model =
    main' []
        [ h1 [] [ text "Elm Conf" ]
        , ul [] (List.map viewRoman model)
        ]


viewRoman : Roman -> Html a
viewRoman roman =
    li [ style [ ( "color", roman.clan.color ) ] ]
        [ text (formattedName roman)
        , viewChildren roman.children
        ]


viewChildren : Children -> Html a
viewChildren (Children children) =
    ul [] (List.map viewChild children)


viewChild : Roman -> Html a
viewChild child =
    li [ style [ ( "color", child.clan.color ) ] ]
        [ text (formattedName child) ]


formattedName : Roman -> String
formattedName roman =
    case roman.name of
        FemaleName cognomen ->
            String.join " " [ roman.clan.name, Maybe.withDefault "" cognomen ]

        MaleName praenomen cognomenM agnomenM ->
            let
                cognomen =
                    Maybe.withDefault "" cognomenM

                agnomen =
                    Maybe.withDefault "" agnomenM

                nomen =
                    (String.dropRight 1 roman.clan.name) ++ "us"
            in
                String.join " " [ praenomen, nomen, cognomen, agnomen ]



-- UPDATE


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
