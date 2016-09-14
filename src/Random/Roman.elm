module Random.Roman exposing (..)

import Roman exposing (..)
import Random.Extra as RandomE
import Random exposing (Generator, andThen)


roman : Id -> Roman -> Generator Roman
roman id father =
    Random.map (Roman id father.clan noChildren) (name father)


name : Roman -> Generator Name
name parent =
    RandomE.choices [ (femaleName parent), maleName ]


femaleName : Roman -> Generator Name
femaleName parent =
    Random.map2 FemaleName (femaleCognomen parent) differentiator


parentalCognomen : Roman -> Maybe String
parentalCognomen parent =
    case parent.name of
        FemaleName cog _ ->
            cog

        MaleName _ cog _ ->
            cog


femaleCognomen : Roman -> Generator (Maybe String)
femaleCognomen parent =
    let
        inheritCog =
            RandomE.constant (parentalCognomen parent)
    in
        RandomE.frequency [ ( 0.8, nothing ), ( 0.2, inheritCog ) ]


differentiator : Generator (Maybe String)
differentiator =
    RandomE.sample [ "Maior", "Minor", "Prima", "Tertia", "Quinta" ]


maleName : Generator Name
maleName =
    cognomen `andThen` nameFromCognomen


nameFromCognomen : Maybe String -> Generator Name
nameFromCognomen cog =
    let
        agnomenGen =
            agnomenFromCognomen cog
    in
        Random.map3 MaleName praenomen (RandomE.constant cog) agnomenGen


agnomenFromCognomen : Maybe String -> Generator (Maybe String)
agnomenFromCognomen cog =
    case cog of
        Just _ ->
            (percentage 0.3 agnomen)

        Nothing ->
            nothing


praenomen : Generator String
praenomen =
    RandomE.sample praenomina
        |> Random.map (Maybe.withDefault "")


cognomen : Generator (Maybe String)
cognomen =
    RandomE.sample cognomina


percentage : Float -> Generator (Maybe a) -> Generator (Maybe a)
percentage percent generator =
    let
        nothingOdds =
            1.0 - percent
    in
        RandomE.frequency [ ( percent, generator ), ( nothingOdds, nothing ) ]


agnomen : Generator (Maybe String)
agnomen =
    RandomE.sample agnomina


nothing : Generator (Maybe a)
nothing =
    RandomE.constant Nothing


praenomina : List String
praenomina =
    [ "Gaius"
    , "Marcus"
    , "Quintus"
    , "Sextus"
    , "Lucius"
    ]


cognomina : List String
cognomina =
    [ "Caesar"
    , "Cicero"
    , "Scipio"
    , "Brutus"
    , "Metellus"
    ]


agnomina : List String
agnomina =
    [ "Victrix"
    , "Africanus"
    , "Felix"
    , "Macedonicus"
    ]
