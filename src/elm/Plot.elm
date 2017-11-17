module Plot exposing (..)

import Html exposing (Html, Attribute, beginnerProgram, div, input)
import Html.Attributes as H exposing (..)
import Html.Events exposing (on, onInput)
import Collage exposing (..)
import Element exposing (..)
import Text exposing (..)
import Color exposing (..)
import Contour exposing (..)


main =
    beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Model =
    { steps : Int
    , contours : Int
    , levelMin : Float
    , levelMax : Float
    }


model : Model
model =
    { steps = 40
    , contours = 5
    , levelMin = 0
    , levelMax = 1
    }


modelGridFunction : Model -> GridFunction
modelGridFunction model =
    let
        grid =
            { min = ( 0, 0 ), max = ( 1, 1 ), steps = model.steps }

        f =
            \( x, y ) -> x * y

        f2 =
            \( x, y ) -> e ^ (-((x - 0.5) ^ 2 + (y - 0.5) ^ 2))

        f3 =
            \( x, y ) -> 2 * x ^ (y - 8)
    in
        gridFunction grid f2



-- UPDATE


type Msg
    = Nop Model
    | UpdateGridSteps String
    | UpdateContours String
    | UpdateLevelMin String
    | UpdateLevelMax String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Nop _ ->
            model

        UpdateGridSteps val ->
            { model | steps = Result.withDefault 0 <| String.toInt <| val }

        UpdateContours val ->
            { model | contours = Result.withDefault 0 <| String.toInt <| val }

        UpdateLevelMin val ->
            let
                newVal =
                    Result.withDefault 0 <| String.toFloat <| val
            in
                { model
                    | levelMin = newVal
                    , levelMax = Basics.max model.levelMax newVal
                }

        UpdateLevelMax val ->
            let
                newVal =
                    Result.withDefault 1 <| String.toFloat <| val
            in
                { model | levelMax = newVal, levelMin = Basics.min model.levelMin newVal }



-- VIEW


myWidth =
    500


myHeight =
    500


view : Model -> Html Msg
view model =
    div []
        [ slider "Grid Size" (toString model.steps) 2 100 UpdateGridSteps
        , slider "Contours" (toString model.contours) 0 80 UpdateContours
        , slider "Level min" (toString model.levelMin) -5 5 UpdateLevelMin
        , slider "Level max" (toString model.levelMax) -5 5 UpdateLevelMax
        , allContours model
        ]


allContours : Model -> Html Msg
allContours model =
    toHtml <|
        collage
            (2 * myWidth)
            (2 * myHeight)
            [ -- , traced (solid Color.blue) <| segment ( 100, 100 ) ( 200, 200 )
              contours model model.levelMin model.levelMax model.contours
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
        gfun =
            modelGridFunction model

        sqs =
            squares gfun.grid

        lines =
            contourLines gfun level
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


slider : String -> String -> Int -> Int -> (String -> Msg) -> Html Msg
slider title val min max update =
    div []
        [ Html.text <| title
        , input
            [ type_ "range"
            , H.min <| toString min
            , H.max <| toString max
            , H.value val
            , onInput update
            ]
            []
        , Html.text <| val
        ]
