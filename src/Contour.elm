module Contour exposing
    ( GridFunction, gridFunction, Point, Grid
    , contourLines, Line, points
    )

{-| This library calculate contour level lines for a two-dimensional scalar field,
based on the Marching Squares algorithm <https://en.wikipedia.org/wiki/Marching_squares>.


# Definition of a two-dimensional scalar field

@docs GridFunction, gridFunction, Point, Grid


# Calculate contour lines

@docs contourLines, Line, points

-}

import Internal.Contour as Internal


{-| Represent a two-dimensioal scalar field
-}
type alias GridFunction =
    Internal.GridFunction


{-| Represent the discretization grid
-}
type alias Grid =
    Internal.Grid


{-| Represent a point
as (Float,Float)
-}
type alias Point =
    Internal.Point


{-| Construct a two-dimensial scalar field from a function

        f : (Float,Float) -> Float

    taking values in [0,1]x[0,1]

-}
gridFunction : Grid -> (Point -> Float) -> GridFunction
gridFunction =
    Internal.gridFunction


{-| A line within a contour
-}
type alias Line =
    Internal.Line


{-| Calculate contour lines for a given level
-}
contourLines : Float -> GridFunction -> List Line
contourLines =
    Internal.contourLines


{-| Get a line's end points
-}
points : Line -> ( Point, Point )
points (Internal.Line p1 p2) =
    ( p1, p2 )
