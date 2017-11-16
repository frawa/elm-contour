module Main exposing (..)

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
            { min = ( 0, 0 ), max = ( 1, 1 ), steps = 40 }

        f =
            \( x, y ) -> x * y

        f2 =
            \( x, y ) -> 1.5 - e ^ ((x - 0.5) ^ 2 + (y - 0.5) ^ 2)
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
    1500


myHeight =
    1000


view model =
    toHtml <|
        collage myWidth
            myHeight
            [ text <| fromString "Hello"
              -- , traced (solid Color.blue) <| segment ( 100, 100 ) ( 200, 200 )
            , contour model
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


contour : Model -> Form
contour model =
    let
        sqs =
            squares model.gfun.grid

        lines =
            contourLines model.gfun 0
    in
        group <| List.map (lineToPath >> tracePath) lines
