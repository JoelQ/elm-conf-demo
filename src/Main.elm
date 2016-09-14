module Main exposing (..)

import Html.App
import Html exposing (..)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import String
import Dict exposing (Dict)
import Roman exposing (Roman, Name(..), Children(..), Id, caesar, cornelia)
import Random.Roman as RandomR
import Random


type alias Model =
    { romans : Dict Id Roman
    , nextId : Id
    }


initialRomans : Dict Id Roman
initialRomans =
    Dict.fromList [ ( caesar.id, caesar ), ( cornelia.id, cornelia ) ]


initialId : Id
initialId =
    Dict.values initialRomans
        |> List.map .id
        |> List.maximum
        |> Maybe.map ((+) 1)
        |> Maybe.withDefault 1


init : ( Model, Cmd Msg )
init =
    ( Model initialRomans initialId, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    main' []
        [ h1 [] [ text "Elm Conf" ]
        , ul [] (List.map viewRoman (Dict.values model.romans))
        ]


viewRoman : Roman -> Html Msg
viewRoman roman =
    li [ style [ ( "color", roman.clan.color ) ] ]
        [ text (formattedName roman)
        , button [ onClick (GenerateChildFor roman) ] [ text "Bless with child" ]
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
        FemaleName cognomenM differentiatorM ->
            let
                cognomen =
                    Maybe.withDefault "" cognomenM

                differentiator =
                    Maybe.withDefault "" differentiatorM
            in
                String.join " " [ roman.clan.name, cognomen, differentiator ]

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
    | GenerateChildFor Roman
    | Birth Roman Roman


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        GenerateChildFor father ->
            let
                randomRomanGenerator =
                    RandomR.roman model.nextId father

                randomRomanCommand =
                    Random.generate (Birth father) randomRomanGenerator

                incrementedModel =
                    { model | nextId = model.nextId + 1 }
            in
                ( incrementedModel, randomRomanCommand )

        Birth father child ->
            let
                updatedRomans =
                    Dict.update father.id (Maybe.map (appendNewChild child)) model.romans
            in
                ( { model | romans = updatedRomans }, Cmd.none )


appendNewChild : Roman -> Roman -> Roman
appendNewChild child father =
    let
        (Children existingChildren) =
            father.children
    in
        { father | children = Children (child :: existingChildren) }


main : Program Never
main =
    Html.App.program
        { init = init
        , view = view
        , update = update
        , subscriptions = (\_ -> Sub.none)
        }
