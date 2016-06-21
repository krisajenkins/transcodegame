module Common.View exposing (px, pct)


px : Int -> String
px n =
    toString n ++ "px"


pct : Int -> String
pct n =
    toString n ++ "%"
