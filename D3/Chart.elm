module D3.Chart exposing (..)

-- where


type alias Margins =
    { left : Float, right : Float, top : Float, bottom : Float }


type alias Chart d =
    { x : d -> Float
    , y : d -> Float
    , xTick : Float -> String
    , yTick : Float -> String
    , width : Float
    , height : Float
    , margins : Margins
    }



--TODO: split Chart into multiple types {a | ...}
--Make each chart type specify it's exact chart type
--Line : Axis (Linear (Sized a)) or something.
--Reify the series type
--Finish Margin implementation


type alias ChartOption a =
    Chart a -> Chart a


fromOptions : List (ChartOption d) -> Chart d
fromOptions options =
    List.foldl (\f x -> f x) defaultChart options


defaultChart : Chart d
defaultChart =
    { x = (always 0)
    , y = (always 0)
    , yTick = toString
    , xTick = toString
    , width = 400
    , height = 300
    , margins =
        { left = 40
        , right = 40
        , top = 40
        , bottom = 40
        }
    }


x : (d -> Float) -> ChartOption d
x f c =
    { c | x = f }


y : (d -> Float) -> ChartOption d
y f c =
    { c | y = f }


width : Float -> ChartOption d
width w c =
    { c | width = w }


height : Float -> ChartOption d
height h c =
    { c | height = h }


xTick : (Float -> String) -> ChartOption d
xTick f c =
    { c | xTick = f }


yTick : (Float -> String) -> ChartOption d
yTick f c =
    { c | yTick = f }


margins : Margins -> ChartOption d
margins m c =
    { c | margins = m }
