module Series exposing (..)

-- where


type alias Dataset x =
    List (Series x)


type alias Series x =
    { key : String
    , values : List x
    }


map : (a -> b) -> Series a -> Series b
map f { key, values } =
    { key = key, values = List.map f values }
