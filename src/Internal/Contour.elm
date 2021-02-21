module Internal.Contour exposing
    ( GridFunction, gridFunction, Point, Grid
    , contourLines, Line(..)
    , Edge(..), RelativeLine(..), RelativePoint(..), Segment(..), StepSize(..), classify, classifySquares, cornersIndices, edgeMidPoint, fromList, gridIndex, gridSize, index, listGridIndices, markLevel, point, segmentLineForSquare, segmentRelativeLine, segmentsByClass, squareCornerIndex, squares, stepSize, value, valueAt, zeroAt, zeroOnEdgeAt
    )

{-| This library calculate contour level lines for a two-dimensional scalar field,
based on the Marching Squares algorithm <https://en.wikipedia.org/wiki/Marching_squares>.


# Definition of a two-dimensional scalar field

@docs GridFunction, gridFunction, Point, Grid


# Calculate contour lines

@docs contourLines, Line

-}

import Array exposing (Array, get, toList)
import Bitwise exposing (or, shiftLeftBy)
import List exposing (concatMap, range)
import Maybe exposing (withDefault)


type alias GridIndex =
    ( Int, Int )


type alias Point =
    ( Float, Float )


type StepSize
    = Step Float Float


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


{-| Represent a two-dimensioal scalar field
-}
type alias GridFunction =
    GridValues Float


stepSize : Grid -> StepSize
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
    Step (width / n) (height / n)


dimensionSize : Grid -> Int
dimensionSize grid =
    .steps grid + 1


gridSize : Grid -> Int
gridSize grid =
    dimensionSize grid ^ 2


point : Grid -> GridIndex -> Point
point grid ( i, j ) =
    let
        (Step hx hy) =
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


{-| Construct a two-dimensial scalar field from a function

        f : (Float,Float) -> Float

    taking values in [0,1]x[0,1]

-}
gridFunction : Grid -> (Point -> Float) -> GridFunction
gridFunction grid f =
    listGridIndices grid
        |> List.map (f << point grid)
        |> fromList grid


fromList : Grid -> List Float -> GridFunction
fromList grid values =
    { grid = grid
    , values = Array.fromList values
    }


markLevel : GridFunction -> Float -> GridValues Int
markLevel gfun level =
    { grid = gfun.grid
    , values =
        gfun.values
            |> Array.map ((<) level)
            |> Array.map boolToInt
    }


boolToInt : Bool -> Int
boolToInt val =
    if val then
        1

    else
        0


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


cornersIndices : Squares -> Int -> List Int
cornersIndices squares1 index1 =
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
                |> List.map (cornersIndices squares_)
                |> List.map (\corners1 -> List.map (value marked) corners1 |> List.map (withDefault 0))
                |> List.map classify
                |> Array.fromList
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
    = Edge0
    | Edge1
    | Edge2
    | Edge3


corners : Edge -> ( Corner, Corner )
corners edge1 =
    case edge1 of
        Edge0 ->
            ( Corner0, Corner1 )

        Edge1 ->
            ( Corner1, Corner2 )

        Edge2 ->
            ( Corner2, Corner3 )

        Edge3 ->
            ( Corner3, Corner0 )


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
            [ Segment Edge0 Edge3 ]

        14 ->
            [ Segment Edge0 Edge3 ]

        2 ->
            [ Segment Edge0 Edge1 ]

        13 ->
            [ Segment Edge0 Edge1 ]

        3 ->
            [ Segment Edge1 Edge3 ]

        12 ->
            [ Segment Edge1 Edge3 ]

        4 ->
            [ Segment Edge1 Edge2 ]

        11 ->
            [ Segment Edge1 Edge2 ]

        5 ->
            [ Segment Edge0 Edge1, Segment Edge2 Edge3 ]

        6 ->
            [ Segment Edge0 Edge2 ]

        9 ->
            [ Segment Edge0 Edge2 ]

        7 ->
            [ Segment Edge2 Edge3 ]

        8 ->
            [ Segment Edge2 Edge3 ]

        10 ->
            [ Segment Edge0 Edge3, Segment Edge1 Edge2 ]

        -- make impossible
        _ ->
            []


cornerGridIndex : Squares -> Int -> Corner -> GridIndex
cornerGridIndex squares1 square corner =
    let
        ( a, b ) =
            gridIndex squares1 square

        ( i, j ) =
            case corner of
                Corner0 ->
                    ( 0, 0 )

                Corner1 ->
                    ( 1, 0 )

                Corner2 ->
                    ( 1, 1 )

                Corner3 ->
                    ( 0, 1 )
    in
    ( a + i, b + j )


zeroOnEdgeAt : GridFunction -> Int -> Edge -> Float
zeroOnEdgeAt gfun square edge1 =
    let
        ( corner1, corner2 ) =
            corners edge1

        sqs =
            squares gfun.grid

        a =
            valueAt gfun (cornerGridIndex sqs square corner1)

        b =
            valueAt gfun (cornerGridIndex sqs square corner2)
    in
    zeroAt a b



{-
   solution of f x = 0
   where f 0 = a, f 1 = b
-}


zeroAt : Float -> Float -> Float
zeroAt a b =
    if a == b then
        0

    else
        -a / (b - a)


{-| A line within a contour
-}
type Line
    = Line Point Point


type RelativeLine
    = RelativeLine RelativePoint RelativePoint


type RelativePoint
    = RelativePoint Float Float


segmentRelativeLine : (Edge -> RelativePoint) -> Segment -> RelativeLine
segmentRelativeLine getPoint segment =
    case segment of
        Segment edge1 edge2 ->
            RelativeLine (getPoint edge1) (getPoint edge2)


edgeMidPoint : Edge -> RelativePoint
edgeMidPoint =
    edgeRelativePoint 0.5


edgeRelativePoint : Float -> Edge -> RelativePoint
edgeRelativePoint lambda edge1 =
    let
        ( corner1, corner2 ) =
            corners edge1
    in
    relativePoint lambda corner1 corner2


relativePoint : Float -> Corner -> Corner -> RelativePoint
relativePoint lambda corner1 corner2 =
    let
        (RelativePoint x1 y1) =
            cornerPoint corner1

        (RelativePoint x2 y2) =
            cornerPoint corner2
    in
    RelativePoint (relative lambda x1 x2) (relative lambda y1 y2)


cornerPoint : Corner -> RelativePoint
cornerPoint corner =
    case corner of
        Corner0 ->
            RelativePoint 0 0

        Corner1 ->
            RelativePoint 1 0

        Corner2 ->
            RelativePoint 1 1

        Corner3 ->
            RelativePoint 0 1


relative : Float -> Float -> Float -> Float
relative lambda x y =
    x + lambda * (y - x)


segmentLineForSquare : (Int -> Edge -> RelativePoint) -> Squares -> Int -> Segment -> Line
segmentLineForSquare getEdgePoint squares1 square segment =
    let
        grid =
            { squares1 | steps = squares1.steps + 1 }
    in
    squareCornerIndex squares1 square
        |> gridIndex grid
        |> point grid
        |> absoluteLine (stepSize grid) (segmentRelativeLine (getEdgePoint square) segment)


absoluteLine : StepSize -> RelativeLine -> Point -> Line
absoluteLine step (RelativeLine rp1 rp2) p =
    Line (absolutePoint step rp1 p) (absolutePoint step rp2 p)


absolutePoint : StepSize -> RelativePoint -> Point -> Point
absolutePoint (Step hx hy) (RelativePoint rx ry) ( x, y ) =
    ( x + hx * rx, y + hy * ry )


{-| Calculate contour lines for a given level
-}
contourLines : Float -> GridFunction -> List Line
contourLines level gfun =
    let
        classified =
            classifySquares gfun level

        gfunLeveled =
            gridMap ((-) level) gfun

        segments =
            gridMap segmentsByClass classified

        toLine =
            segmentLineForSquare (edgeZeroPoint gfunLeveled) segments.grid

        toLines =
            \i ->
                List.map (toLine i)

        lines =
            gridMapIndexed toLines segments
    in
    concatMap identity <| toList <| .values lines


edgeZeroPoint : GridFunction -> Int -> Edge -> RelativePoint
edgeZeroPoint gfun square edge =
    let
        lambda =
            zeroOnEdgeAt gfun square edge
    in
    edgeRelativePoint lambda edge
