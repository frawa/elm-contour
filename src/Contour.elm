module Contour exposing
    ( GridFunction, gridFunction, Point, Grid
    , contourLines, Line, points
    , viewGridFunction, drawGridFunction, traceLine, drawContour, Style, defaultStyle
    , fromList, pointAt
    )

{-| This library calculate contour level lines for a two-dimensional scalar field,
based on the Marching Squares algorithm <https://en.wikipedia.org/wiki/Marching_squares>.


# Definition of a two-dimensional scalar field

@docs GridFunction, gridFunction, Point, Grid, pointAt, fromList


# Calculate contour lines

@docs contourLines, Line, points


# Draw contour lines

@docs viewGridFunction, drawGridFunction, traceLine, drawContour, Style, defaultStyle

-}

import Collage
import Collage.Render as Render
import Html
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


{-| Construct a two-dimensial scalar field from a data in a list of values

    Values are expected in the order of indices in the given grid,
    running from min to max, incrementing the first component first.

    Use pointAt to get (x,y) coordinates for an data index.

-}
fromList : Grid -> List Float -> GridFunction
fromList =
    Internal.fromList


{-| Get the point, ithat is (x,y) coordinates, in the grid for a data index.
-}
pointAt : Grid -> Int -> Point
pointAt grid index =
    Internal.gridIndex grid index |> Internal.point grid


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


{-| Render SVG with contours for several levels of a grid function.
-}
viewGridFunction : Style -> GridFunction -> List Float -> Html.Html msg
viewGridFunction style gridFun levels =
    drawGridFunction style gridFun levels
        |> Render.svg


{-| Rendering style properties.
-}
type alias Style =
    { width : Int
    , height : Int
    , lineStyle : Collage.LineStyle
    }


{-| Default style.
-}
defaultStyle : Style
defaultStyle =
    { width = 500, height = 500, lineStyle = Collage.defaultLineStyle }


{-| Draw contour for one level of a grid function.
-}
drawContour : Style -> GridFunction -> Float -> Collage.Collage msg
drawContour style gridFun level =
    contourLines level gridFun
        |> List.map (traceLine style)
        |> Collage.group


{-| Draw contours for several levels of a grid function.
-}
drawGridFunction : Style -> GridFunction -> List Float -> Collage.Collage msg
drawGridFunction style gridFun levels =
    levels
        |> List.map (drawContour style gridFun)
        |> Collage.group


{-| Draw a line as a traced path.
-}
traceLine : Style -> Line -> Collage.Collage msg
traceLine style line =
    let
        scaled : Point -> Point -> Point
        scaled =
            \( w, h ) ( x, y ) -> ( w * x, h * y )

        scale =
            scaled ( toFloat style.width, toFloat style.height )

        ( p1, p2 ) =
            points line
    in
    Collage.segment (scale p1) (scale p2)
        |> Collage.traced style.lineStyle
