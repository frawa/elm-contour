module Contour exposing (..)

import Array exposing (Array, get, fromList, map, foldl)
import Maybe exposing (withDefault)
import List exposing (map, range)
import Bitwise exposing (or, shiftLeftBy)


type alias Position =
    ( Int, Int )


type alias Point =
    ( Float, Float )


type alias Grid =
    { steps : Int
    , min : Position
    , max : Position
    }


type alias Squares =
    Grid


type alias GridValues a =
    { grid : Grid
    , values : Array a
    }


type alias GridFunction =
    GridValues Float


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
    List.map (position grid) <| range 0 ((gridSize grid) - 1)


valueAt : GridFunction -> Position -> Float
valueAt gfun coor =
    withDefault 0 <|
        value gfun (index (.grid gfun) coor)


value : GridValues a -> Int -> Maybe a
value gfun index =
    get index (.values gfun)


gridFunction : Grid -> (Point -> Float) -> GridFunction
gridFunction grid f =
    { grid = grid, values = fromList <| List.map (f << (point grid)) (listPositions grid) }


markLevel : GridFunction -> Float -> GridValues Int
markLevel gfun level =
    { gfun
        | values =
            Array.map
                (\more ->
                    if more then
                        1
                    else
                        0
                )
            <|
                Array.map ((<) level) gfun.values
    }


squares : Grid -> Squares
squares grid =
    { grid | steps = grid.steps - 1 }


squareCornerIndex : Squares -> Int -> Int
squareCornerIndex squares i =
    let
        grid =
            { squares | steps = squares.steps + 1 }
    in
        index grid <| position squares i


corners : Squares -> Int -> List Int
corners squares index =
    let
        n =
            dimensionSize squares + 1

        corner =
            squareCornerIndex squares index
    in
        [ corner, corner + 1, corner + n + 1, corner + n ]


classify : List Int -> Int
classify marked =
    List.foldl
        (\mark acc ->
            or mark (shiftLeftBy 1 acc)
        )
        0
        marked


classifySquares : GridFunction -> Float -> GridValues Int
classifySquares gfun level =
    let
        marked =
            markLevel gfun level

        squares_ =
                squares <|
                    .grid marked
    in
        { marked
            | grid = squares_
            , values =
                listPositions squares_
                    |> List.map (index squares_)
                    |> List.map (corners squares_)
                    |> List.map (\corners -> List.map (value marked) corners |> List.map (withDefault 0))
                    |> List.map classify
                    |> fromList
        }
