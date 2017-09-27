module Contour exposing (..)

import Array exposing (Array, get, fromList)
import Maybe exposing (withDefault)
import List exposing (map, range)


type alias Position =
    ( Int, Int )


type alias Point =
    ( Float, Float )


type alias Grid =
    { steps : Int
    , min : Position
    , max : Position
    }


type alias GridFunction =
    { grid : Grid
    , values : Array Float
    }


stepSize : Grid -> Point
stepSize grid =
    let
        ( xmin, ymin ) =
            .min grid

        ( xmax, ymax ) =
            .max grid

        n =
            toFloat <| .steps grid

        width =
            toFloat <| xmax - xmin

        height =
            toFloat <| ymax - ymin
    in
        ( width / n, height / n )


dimensionSize : Grid -> Int
dimensionSize grid =
    (.steps grid + 1)


gridSize : Grid -> Int
gridSize grid =
    (dimensionSize grid) ^ 2


point : Grid -> Position -> Point
point grid ( i, j ) =
    let
        ( hx, hy ) =
            stepSize grid
    in
        ( (toFloat i) * hx, (toFloat j) * hy )


index : Grid -> Position -> Int
index grid ( i, j ) =
    let
        n =
            dimensionSize grid
    in
        j * n + i


position : Grid -> Int -> Position
position grid index =
    let
        n =
            dimensionSize grid
    in
        ( index % n, index // n )


listPositions : Grid -> List Position
listPositions grid =
    map (position grid) <| range 0 ((gridSize grid) - 1)


valueAt : GridFunction -> Position -> Float
valueAt gfun coor =
    withDefault 0 <|
        get (index (.grid gfun) coor) (.values gfun)


gridFunction : Grid -> (Point -> Float) -> GridFunction
gridFunction grid f =
    { grid = grid, values = fromList <| map (f << (point grid)) (listPositions grid) }
