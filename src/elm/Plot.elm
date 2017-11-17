module Plot exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, div, input)


-- import Html.Attributes exposing (..)
-- import Html.Events exposing (onInput)

import Collage exposing (..)
import Element exposing (..)
import Text exposing (..)
import Color exposing (..)
import Contour exposing (..)


main =
    beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { gfun : GridFunction
    }


model : Model
model =
    let
        grid =
            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 80 }

        f =
            \( x, y ) -> x * y

        f2 =
            \( x, y ) -> e ^ (-((x - 0.5) ^ 2 + (y - 0.5) ^ 2))

        f3 =
            \( x, y ) -> 2 * x ^ (y - 8)
    in
        { gfun = gridFunction grid f2
        }



-- UPDATE


type Msg
    = Nop Model


update (Nop model) oldContent =
    model



-- VIEW


myWidth =
    500


myHeight =
    500


view model =
    toHtml <|
        collage
            (2 * myWidth)
            (2 * myHeight)
            [ -- , traced (solid Color.blue) <| segment ( 100, 100 ) ( 200, 200 )
              contour model 0.8
            , contours model 0 1 50
            , text <| fromString "Hello"
            ]


scaled : Point -> Point -> Point
scaled ( w, h ) ( x, y ) =
    ( w * x, h * y )


lineToPath : Contour.Line -> Path
lineToPath line =
    let
        scale =
            scaled ( myWidth, myHeight )
    in
        case line of
            Line p1 p2 ->
                segment (scale p1) (scale p2)


tracePath : Path -> Form
tracePath =
    traced (solid Color.blue)


contour : Model -> Float -> Form
contour model level =
    let
        sqs =
            squares model.gfun.grid

        lines =
            contourLines model.gfun level
    in
        group <| List.map (lineToPath >> tracePath) lines


contours : Model -> Float -> Float -> Int -> Form
contours model min max steps =
    let
        delta =
            (max - min) / toFloat steps

        level =
            \i -> min + toFloat i * delta
    in
        group <|
            List.map (contour model) <|
                List.map level <|
                    List.range 0 (steps - 1)
