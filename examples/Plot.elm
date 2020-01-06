module Plot exposing (..)

import Browser
import Collage exposing (Collage, Path, Point, defaultLineStyle, group, rendered, segment, traced)
import Collage.Layout exposing (vertical)
import Collage.Render exposing (svgBox)
import Collage.Text exposing (fromString)
import Contour exposing (GridFunction, Line, contourLines, gridFunction)
import Html exposing (Attribute, Html, div, input, option, select)
import Html.Attributes as H exposing (..)
import Html.Events exposing (onInput)
import String exposing (fromFloat, fromInt)


main =
    Browser.sandbox { init = model0, view = view, update = update }



-- MODEL


type alias Model =
    { steps : Int
    , contours : Int
    , levelMin : Float
    , levelMax : Float
    , function : Function
    }


model0 : Model
model0 =
    { steps = 40
    , contours = 5
    , levelMin = 0
    , levelMax = 1
    , function = defaultFunction
    }


type alias Function =
    { name : String
    , f : Point -> Float
    }


defaultFunction : Function
defaultFunction =
    { name = "Cylinder"
    , f =
        \( x, y ) -> e ^ -((x - 0.5) ^ 2 + (y - 0.5) ^ 2)
    }


availableFunctions : List Function
availableFunctions =
    [ defaultFunction
    , { name = "Bilinear"
      , f =
            \( x, y ) -> x * y
      }
    , { name = "f3"
      , f =
            \( x, y ) -> 2 * x ^ (y - 8)
      }
    ]


modelGridFunction : Model -> GridFunction
modelGridFunction model =
    let
        grid =
            { min = ( 0, 0 ), max = ( 1, 1 ), steps = model.steps }
    in
    gridFunction grid model.function.f



-- UPDATE


type Msg
    = Nop Model
    | UpdateGridSteps String
    | UpdateContours String
    | UpdateLevelMin String
    | UpdateLevelMax String
    | UpdateFunction String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Nop _ ->
            model

        UpdateGridSteps val ->
            { model | steps = Maybe.withDefault 0 <| String.toInt <| val }

        UpdateContours val ->
            { model | contours = Maybe.withDefault 0 <| String.toInt <| val }

        UpdateLevelMin val ->
            let
                newVal =
                    Maybe.withDefault 0 <| String.toFloat <| val
            in
            { model
                | levelMin = newVal
                , levelMax = Basics.max model.levelMax newVal
            }

        UpdateLevelMax val ->
            let
                newVal =
                    Maybe.withDefault 1 <| String.toFloat <| val
            in
            { model | levelMax = newVal, levelMin = Basics.min model.levelMin newVal }

        UpdateFunction val ->
            let
                newVal =
                    List.filter (\fun -> fun.name == val) availableFunctions |> List.head |> Maybe.withDefault defaultFunction
            in
            { model | function = newVal }



-- VIEW


myWidth =
    500


myHeight =
    500


view : Model -> Html Msg
view model =
    div []
        [ slider "Grid Size" (fromInt model.steps) 2 100 UpdateGridSteps
        , slider "Contours" (fromInt model.contours) 0 80 UpdateContours
        , slider "Level min" (fromFloat model.levelMin) -5 5 UpdateLevelMin
        , slider "Level max" (fromFloat model.levelMax) -5 5 UpdateLevelMax
        , selectFunction "Function" availableFunctions UpdateFunction
        , allContours model
        ]


allContours : Model -> Html Msg
allContours model =
    svgBox ( 2 * myWidth, 2 * myHeight ) <|
        vertical
            [ -- , traced (solid Color.blue) <| segment ( 100, 100 ) ( 200, 200 )
              contours model model.levelMin model.levelMax model.contours
            , rendered <| fromString "Hello"
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
        Contour.Line p1 p2 ->
            segment (scale p1) (scale p2)


tracePath : Path -> Collage msg
tracePath =
    traced defaultLineStyle


contour : Model -> Float -> Collage msg
contour model level =
    let
        gfun =
            modelGridFunction model

        lines =
            contourLines gfun level
    in
    group <| List.map (lineToPath >> tracePath) lines


contours : Model -> Float -> Float -> Int -> Collage msg
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
slider title val min max update1 =
    div []
        [ Html.text <| title
        , input
            [ type_ "range"
            , H.min <| fromInt min
            , H.max <| fromInt max
            , H.value val
            , onInput update1
            ]
            []
        , Html.text <| val
        ]


selectFunction : String -> List Function -> (String -> Msg) -> Html Msg
selectFunction title functions update1 =
    div []
        [ Html.text <| title
        , select [ onInput update1 ] <|
            List.map
                (\fun ->
                    option [ value fun.name ] [ Html.text fun.name ]
                )
                functions
        ]
