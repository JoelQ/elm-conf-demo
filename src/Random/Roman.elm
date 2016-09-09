module Random.Roman exposing (..)

import Roman exposing (..)
import Random.Extra as RandomE
import Random exposing (Generator)


roman : Id -> Roman -> Generator Roman
roman id father =
    Random.map (Roman id father.clan noChildren) name


name : Generator Name
name =
    RandomE.choices [ femaleName, maleName ]


femaleName : Generator Name
femaleName =
    Random.map FemaleName nothing


maleName : Generator Name
maleName =
    Random.map3 MaleName praenomen cognomen agnomen


praenomen : Generator String
praenomen =
    RandomE.sample praenomina
        |> Random.map (Maybe.withDefault "")


cognomen : Generator (Maybe String)
cognomen =
    RandomE.sample cognomina


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
