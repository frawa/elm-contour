module ContourTest exposing (suite)

import Array exposing (Array, empty)
import Contour exposing (..)
import Expect exposing (Expectation, FloatingPointTolerance(..))
import Fuzz exposing (intRange, tuple)
import List exposing (length)
import Test exposing (..)


eps =
    Absolute 1.0e-4


suite : Test
suite =
    describe "Contour"
        [ describe "Grid"
            [ test "first" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 10 }
                    in
                    Expect.all
                        [ Expect.equal 10 << .steps
                        , Expect.equal ( 0, 0 ) << .min
                        , Expect.equal ( 1, 1 ) << .max
                        , Expect.equal (Step 0.1 0.1) << stepSize
                        , Expect.equal 121 << gridSize
                        ]
                        grid
            , test "points" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 10 }
                    in
                    Expect.equal
                        [ ( 0.0, 0.0 )
                        , ( 0.5, 0.5 )
                        , ( 1.0, 1.0 )
                        ]
                        [ point grid ( 0, 0 )
                        , point grid ( 5, 5 )
                        , point grid ( 10, 10 )
                        ]
            , test "index" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 10 }
                    in
                    Expect.equal
                        [ 0
                        , 60
                        , 108
                        ]
                        [ index grid ( 0, 0 )
                        , index grid ( 5, 5 )
                        , index grid ( 9, 9 )
                        ]
            , test "position" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 10 }
                    in
                    Expect.equal
                        [ ( 0, 0 )
                        , ( 0, 5 )
                        , ( 0, 9 )
                        ]
                        [ gridIndex grid 0
                        , gridIndex grid 55
                        , gridIndex grid 99
                        ]
            , test "list positions" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 1 }
                    in
                    Expect.equal
                        [ ( 0, 0 )
                        , ( 1, 0 )
                        , ( 0, 1 )
                        , ( 1, 1 )
                        ]
                        (listGridIndices grid)
            ]
        , describe "GridFunction"
            [ fuzz (tuple ( intRange 0 10, intRange 0 10 )) "zero function" <|
                \pos ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 10 }

                        gfun =
                            { grid = grid, values = empty }
                    in
                    Expect.equal 0 (valueAt gfun pos)
            , fuzz (tuple ( intRange 0 10, intRange 0 10 )) "simple function" <|
                \pos ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 10 }

                        f =
                            \( x, y ) -> x * y

                        gfun =
                            gridFunction grid f
                    in
                    Expect.within eps
                        (f (point grid pos))
                        (valueAt gfun pos)
            , fuzz (tuple ( intRange 0 10, intRange 0 10 )) "simple function by index" <|
                \pos ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 10 }

                        f =
                            \( x, y ) -> x * y

                        gfun =
                            gridFunction grid f
                    in
                    Expect.within eps
                        (f (point grid pos))
                        (value gfun (index grid pos) |> Maybe.withDefault 0)
            ]
        , describe "marching squares"
            [ test "mark level" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 2 }

                        gfun =
                            { grid = grid
                            , values =
                                Array.fromList
                                    [ 0
                                    , 0
                                    , 0
                                    , 0
                                    , 9
                                    , 0
                                    , 0
                                    , 0
                                    , 0
                                    ]
                            }
                    in
                    Expect.equal
                        (Array.fromList
                            [ 0
                            , 0
                            , 0
                            , 0
                            , 1
                            , 0
                            , 0
                            , 0
                            , 0
                            ]
                        )
                        (.values <| markLevel gfun 5)
            , test "squares" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 2 }
                    in
                    Expect.equal { grid | steps = 1 } (squares grid)
            , test "a squares corner index in the grid" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 3 }

                        boxes =
                            squares grid
                    in
                    Expect.equal
                        [ 0
                        , 4
                        ]
                        [ squareCornerIndex boxes 0
                        , squareCornerIndex boxes 3
                        ]
            , test "corners" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 2 }

                        boxes =
                            squares grid
                    in
                    Expect.equal
                        [ [ 0, 1, 4, 3 ]
                        , [ 4, 5, 8, 7 ]
                        ]
                        [ cornersIndices boxes 0
                        , cornersIndices boxes 3
                        ]
            , test "classify one square" <|
                \_ ->
                    Expect.equal
                        [ 0
                        , 1
                        , 2
                        , 4
                        , 8
                        , 15
                        ]
                        [ classify [ 0, 0, 0, 0 ]
                        , classify [ 1, 0, 0, 0 ]
                        , classify [ 0, 1, 0, 0 ]
                        , classify [ 0, 0, 1, 0 ]
                        , classify [ 0, 0, 0, 1 ]
                        , classify [ 1, 1, 1, 1 ]
                        ]
            , test "classify squares" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 2 }

                        gfun =
                            { grid = grid
                            , values =
                                Array.fromList
                                    [ 0
                                    , 0
                                    , 0
                                    , 0
                                    , 9
                                    , 0
                                    , 0
                                    , 0
                                    , 0
                                    ]
                            }
                    in
                    Expect.equal
                        (Array.fromList
                            [ 4
                            , 8
                            , 2
                            , 1
                            ]
                        )
                        (.values <| classifySquares gfun 5)
            , test "classified squares" <|
                \_ ->
                    Expect.equal
                        [ segmentsByClass 15
                        , segmentsByClass 14
                        , segmentsByClass 13
                        , segmentsByClass 12
                        , segmentsByClass 11
                        , segmentsByClass 9
                        , segmentsByClass 8
                        ]
                        [ segmentsByClass 0
                        , segmentsByClass 1
                        , segmentsByClass 2
                        , segmentsByClass 3
                        , segmentsByClass 4
                        , segmentsByClass 6
                        , segmentsByClass 7
                        ]
            , test "classified squares, more" <|
                \_ ->
                    Expect.equal
                        [ 2
                        , 2
                        , 0
                        , 1
                        ]
                        [ length <| segmentsByClass 10
                        , length <| segmentsByClass 5
                        , length <| segmentsByClass 0
                        , length <| segmentsByClass 1
                        ]
            , test "zero at" <|
                \_ ->
                    Expect.equal
                        [ 0.5
                        , 0.25
                        ]
                        [ zeroAt -1 1
                        , zeroAt -1 3
                        ]
            , test "zero on edge" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 1 }

                        gfun =
                            { grid = grid
                            , values =
                                Array.fromList
                                    [ -1
                                    , 3
                                    , 3
                                    , -1
                                    ]
                            }
                    in
                    Expect.equal
                        [ -1
                        , 3
                        , 0.25
                        , 0.75
                        , 0.25
                        , 0.75
                        ]
                        [ valueAt gfun ( 0, 0 )
                        , valueAt gfun ( 0, 1 )
                        , zeroOnEdgeAt gfun 0 Edge0
                        , zeroOnEdgeAt gfun 0 Edge1
                        , zeroOnEdgeAt gfun 0 Edge2
                        , zeroOnEdgeAt gfun 0 Edge3
                        ]
            , test "segment offset" <|
                \_ ->
                    Expect.equal
                        [ RelativeLine (RelativePoint 0.5 0.0) (RelativePoint 1.0 0.5)
                        , RelativeLine (RelativePoint 1.0 0.5) (RelativePoint 0.5 1.0)
                        ]
                        [ segmentRelativeLine edgeMidPoint (Segment Edge0 Edge1)
                        , segmentRelativeLine edgeMidPoint (Segment Edge1 Edge2)
                        ]
            , test "segment line" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 1 }

                        sqs =
                            squares grid
                    in
                    Expect.equal
                        [ Line ( 0.5, 0.0 ) ( 1.0, 0.5 )
                        , Line ( 1.0, 0.5 ) ( 0.5, 1.0 )
                        , Line ( 0.5, 1.0 ) ( 0.0, 0.5 )
                        , Line ( 0.0, 0.5 ) ( 0.5, 0.0 )
                        ]
                        [ segmentLineForSquare (always edgeMidPoint) sqs 0 (Segment Edge0 Edge1)
                        , segmentLineForSquare (always edgeMidPoint) sqs 0 (Segment Edge1 Edge2)
                        , segmentLineForSquare (always edgeMidPoint) sqs 0 (Segment Edge2 Edge3)
                        , segmentLineForSquare (always edgeMidPoint) sqs 0 (Segment Edge3 Edge0)
                        ]
            , test "segment line finer" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 2 }

                        sqs =
                            squares grid
                    in
                    Expect.equal
                        [ Line ( 0.75, 0.0 ) ( 1.0, 0.25 )
                        , Line ( 0.5, 0.75 ) ( 0.25, 1.0 )
                        ]
                        [ segmentLineForSquare (always edgeMidPoint) sqs 1 (Segment Edge0 Edge1)
                        , segmentLineForSquare (always edgeMidPoint) sqs 2 (Segment Edge1 Edge2)
                        ]
            , test "contour lines" <|
                \_ ->
                    let
                        grid =
                            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 3 }

                        gfun =
                            gridFunction grid (\( x, y ) -> x * y)

                        lines =
                            contourLines 0.0 gfun
                    in
                    Expect.equal
                        [ [ Line ( 0.3333333333333333, 0 ) ( 0, 0.3333333333333333 )
                          , Line ( 0.6666666666666666, 0 ) ( 0.3333333333333333, 0 )
                          , Line ( 1, 0 ) ( 0.6666666666666666, 0 )
                          , Line ( 0, 0.3333333333333333 ) ( 0, 0.6666666666666666 )
                          , Line ( 0, 0.6666666666666666 ) ( 0, 1 )
                          ]
                        ]
                        [ lines ]
            ]
        ]
