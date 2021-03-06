module Roman
    exposing
        ( Name(..)
        , Children(..)
        , Clan
        , Id
        , Roman
        , noChildren
        , caesar
        , cornelia
        , markAntony
        , brutus
        )


type Name
    = MaleName String (Maybe String) (Maybe String)
    | FemaleName (Maybe String) (Maybe String)


type alias Clan =
    { name : String
    , color : String
    }


type alias Id =
    Int


type alias Roman =
    { id : Id
    , clan : Clan
    , children : Children
    , name : Name
    }


type Children
    = Children (List Roman)


noChildren : Children
noChildren =
    Children []


caesar : Roman
caesar =
    let
        clan =
            Clan "Julia" "red"

        name =
            MaleName "Gaius" (Just "Caesar") Nothing
    in
        Roman 1 clan (Children [ julia ]) name


julia : Roman
julia =
    Roman 3 (Clan "Julia" "red") noChildren (FemaleName Nothing Nothing)


cornelia : Roman
cornelia =
    let
        clan =
            Clan "Cornelia" "green"

        name =
            FemaleName (Just "Africana") (Just "Minor")
    in
        Roman 2 clan noChildren name


markAntony : Roman
markAntony =
    Roman 4 (Clan "Antonia" "blue") noChildren (MaleName "Marcus" Nothing Nothing)


brutus : Roman
brutus =
    let
        name =
            MaleName "Marcus" (Just "Brutus") Nothing
    in
        Roman 5 (Clan "Junia" "purple") noChildren name
