module Contour exposing (..)

import Array exposing (Array, foldl, fromList, get, map, toList)
import Bitwise exposing (or, shiftLeftBy)
import List exposing (concatMap, map, range)
import Maybe exposing (withDefault)


type alias GridIndex =
    ( Int, Int )


type alias Point =
    ( Float, Float )


type alias Grid =
    { steps : Int
    , min : GridIndex
    , max : GridIndex
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
    .steps grid + 1


gridSize : Grid -> Int
gridSize grid =
    dimensionSize grid ^ 2


point : Grid -> GridIndex -> Point
point grid ( i, j ) =
    let
        ( hx, hy ) =
            stepSize grid
    in
    ( toFloat i * hx, toFloat j * hy )


index : Grid -> GridIndex -> Int
index grid ( i, j ) =
    let
        n =
            dimensionSize grid
    in
    j * n + i


gridIndex : Grid -> Int -> GridIndex
gridIndex grid index1 =
    let
        n =
            dimensionSize grid
    in
    ( modBy n index1, index1 // n )


listGridIndices : Grid -> List GridIndex
listGridIndices grid =
    List.map (gridIndex grid) <| range 0 (gridSize grid - 1)


valueAt : GridFunction -> GridIndex -> Float
valueAt gfun coor =
    withDefault 0 <|
        value gfun (index (.grid gfun) coor)


value : GridValues a -> Int -> Maybe a
value gfun index1 =
    get index1 (.values gfun)


gridMapIndexed : (Int -> a -> b) -> GridValues a -> GridValues b
gridMapIndexed f gvals =
    { grid = gvals.grid
    , values = Array.fromList <| List.map (\( a, b ) -> f a b) <| Array.toIndexedList gvals.values
    }


gridMap : (a -> b) -> GridValues a -> GridValues b
gridMap f gvals =
    { grid = gvals.grid
    , values = Array.map f gvals.values
    }


gridFunction : Grid -> (Point -> Float) -> GridFunction
gridFunction grid f =
    { grid = grid, values = fromList <| List.map (f << point grid) (listGridIndices grid) }


markLevel : GridFunction -> Float -> GridValues Int
markLevel gfun level =
    { grid = gfun.grid
    , values =
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
squareCornerIndex squares1 i =
    let
        grid =
            { squares1 | steps = squares1.steps + 1 }
    in
    index grid <| gridIndex squares1 i


corners2 : Squares -> Int -> List Int
corners2 squares1 index1 =
    let
        n =
            dimensionSize squares1 + 1

        corner =
            squareCornerIndex squares1 index1
    in
    [ corner, corner + 1, corner + n + 1, corner + n ]


classify : List Int -> Int
classify marked =
    List.foldr
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
            listGridIndices squares_
                |> List.map (index squares_)
                |> List.map (corners2 squares_)
                |> List.map (\corners -> List.map (value marked) corners |> List.map (withDefault 0))
                |> List.map classify
                |> fromList
    }



-- corners of a square
--
-- 3 -- 2
-- |    |
-- 0 -- 1
--
--
-- edges of a square
--
-- + -- 2 -- +
-- |         |
-- 3         1
-- |         |
-- + -- 0 -- +
--
-- segments of a square
-- (are the two edges cut by a contour)
--
-- see https://en.wikipedia.org/wiki/Marching_squares


type Corner
    = Corner0
    | Corner1
    | Corner2
    | Corner3


type Edge
    = Edge Corner Corner


edge : Int -> Edge
edge i =
    case i of
        0 ->
            Edge Corner0 Corner1

        1 ->
            Edge Corner1 Corner2

        2 ->
            Edge Corner2 Corner3

        3 ->
            Edge Corner3 Corner0

        -- make impossible
        _ ->
            Edge Corner0 Corner0



-- edges : Array Edge
-- edges =
--     fromList [ edge 0 0, edge 0 1, edge 1 1, edge 1 0 ]
-- (!) : Array a -> Int -> a
-- (!) arr i =
--     withDefault get i arr
-- infixr 9 !


type Segment
    = Segment Edge Edge


segmentsByClass : Int -> List Segment
segmentsByClass class =
    case class of
        0 ->
            []

        15 ->
            []

        1 ->
            [ Segment (edge 0) (edge 3) ]

        14 ->
            [ Segment (edge 0) (edge 3) ]

        2 ->
            [ Segment (edge 0) (edge 1) ]

        13 ->
            [ Segment (edge 0) (edge 1) ]

        3 ->
            [ Segment (edge 1) (edge 3) ]

        12 ->
            [ Segment (edge 1) (edge 3) ]

        4 ->
            [ Segment (edge 1) (edge 2) ]

        11 ->
            [ Segment (edge 1) (edge 2) ]

        5 ->
            [ Segment (edge 0) (edge 1), Segment (edge 2) (edge 3) ]

        6 ->
            [ Segment (edge 0) (edge 2) ]

        9 ->
            [ Segment (edge 0) (edge 2) ]

        7 ->
            [ Segment (edge 2) (edge 3) ]

        8 ->
            [ Segment (edge 2) (edge 3) ]

        10 ->
            [ Segment (edge 0) (edge 3), Segment (edge 1) (edge 2) ]

        -- make impossible
        _ ->
            []


cornerGridIndex : Squares -> Int -> Corner -> GridIndex
cornerGridIndex squares1 square corner =
    let
        index1 =
            squareCornerIndex squares1 square

        ( i, j ) =
            case corner of
                Corner0 ->
                    ( 0, 0 )

                Corner1 ->
                    ( 0, 1 )

                Corner2 ->
                    ( 1, 1 )

                Corner3 ->
                    ( 1, 0 )
    in
    ( index1 + i, index1 + j )


zeroOnEdgeAt : GridFunction -> Int -> Edge -> Float
zeroOnEdgeAt gfun square edge1 =
    case edge1 of
        Edge corner1 corner2 ->
            let
                sqs =
                    squares gfun.grid

                a =
                    valueAt gfun (cornerGridIndex sqs square corner1)

                b =
                    valueAt gfun (cornerGridIndex sqs square corner2)
            in
            zeroAt a b



-- solution of f x = 0
-- where f 0 = a, f 1 = b


zeroAt : Float -> Float -> Float
zeroAt a b =
    -a / (b - a)


type Line
    = Line Point Point


segmentLineOffset : Segment -> Line
segmentLineOffset segment =
    case segment of
        Segment edge1 edge2 ->
            Line (edgeMidPointOffset edge1) (edgeMidPointOffset edge2)


edgeMidPointOffset : Edge -> Point
edgeMidPointOffset edge1 =
    case edge1 of
        Edge corner1 corner2 ->
            midPointOffset corner1 corner2


midPointOffset : Corner -> Corner -> Point
midPointOffset corner1 corner2 =
    let
        ( x1, y1 ) =
            cornerOffset corner1

        ( x2, y2 ) =
            cornerOffset corner2
    in
    ( mid x1 x2, mid y1 y2 )


cornerOffset : Corner -> Point
cornerOffset corner =
    case corner of
        Corner0 ->
            ( 0, 0 )

        Corner1 ->
            ( 1, 0 )

        Corner2 ->
            ( 1, 1 )

        Corner3 ->
            ( 0, 1 )


mid : Float -> Float -> Float
mid x y =
    (x + y) / 2


segmentLine : Squares -> Int -> Segment -> Line
segmentLine squares1 square segment =
    let
        grid =
            { squares1 | steps = squares1.steps + 1 }
    in
    squareCornerIndex squares1 square
        |> gridIndex grid
        |> point grid
        |> offsetLine (stepSize grid) (segmentLineOffset segment)


offsetLine : Point -> Line -> Point -> Line
offsetLine h line p =
    case line of
        Line p1 p2 ->
            Line (offsetPoint p h p1) (offsetPoint p h p2)


offsetPoint : Point -> Point -> Point -> Point
offsetPoint ( x, y ) ( hx, hy ) ( ox, oy ) =
    ( x + hx * ox, y + hy * oy )


contourLines : GridFunction -> Float -> List Line
contourLines gfun level =
    let
        classified =
            classifySquares gfun level

        segments =
            gridMap segmentsByClass classified

        toLine =
            segmentLine segments.grid

        toLines =
            \i ->
                List.map (toLine i)

        lines =
            gridMapIndexed toLines segments
    in
    concatMap identity <| toList <| .values lines
