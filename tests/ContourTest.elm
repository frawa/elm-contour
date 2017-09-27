module ContourTest exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (intRange, tuple)
import Test exposing (..)
import Array exposing (Array, empty)
import Contour exposing (..)


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
                            , Expect.equal ( 0.1, 0.1 ) << stepSize
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
                            [ position grid 0
                            , position grid 55
                            , position grid 99
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
                            (listPositions grid)
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
                        Expect.equal (f (point grid pos)) (valueAt gfun pos)
            ]
        ]
