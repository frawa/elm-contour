module Plot exposing (..)

import Browser
import Collage exposing (Point)
import Contour exposing (GridFunction, defaultStyle, gridFunction, viewGridFunction)
import Html exposing (Html, div, input, option, select, text)
import Html.Attributes as HA
import Html.Events as HE
import String exposing (fromFloat, fromInt)


main : Program () Model Msg
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
    let
        gridFun =
            modelGridFunction model

        style =
            defaultStyle

        steps =
            model.contours

        min =
            model.levelMin

        max =
            model.levelMax

        delta =
            (max - min) / toFloat steps

        toLevel =
            \i -> min + toFloat i * delta

        levels =
            List.range 0 (model.contours - 1) |> List.map toLevel
    in
    viewGridFunction style gridFun levels


slider : String -> String -> Int -> Int -> (String -> Msg) -> Html Msg
slider title val min max update1 =
    div []
        [ text <| title
        , input
            [ HA.type_ "range"
            , HA.min <| fromInt min
            , HA.max <| fromInt max
            , HA.value val
            , HE.onInput update1
            ]
            []
        , Html.text <| val
        ]


selectFunction : String -> List Function -> (String -> Msg) -> Html Msg
selectFunction title functions update1 =
    div []
        [ text <| title
        , select [ HE.onInput update1 ] <|
            List.map
                (\fun ->
                    option [ HA.value fun.name ] [ Html.text fun.name ]
                )
                functions
        ]
