module Main exposing (..)

import Browser
import Html exposing (Html, a, div, h1, ol, text)
import Html.Attributes exposing (href)


type alias Model =
    {}


type Msg
    = Boolean


view : Model -> Html Msg
view _ =
    div
        []
        [ h1
            []
            [ text "Contour Examples" ]
        , ol
            []
            [ a
                [ href "./Plot.html" ]
                [ text "Plot.elm" ]
            ]
        ]


update : Msg -> Model -> Model
update _ model =
    model


main =
    Browser.sandbox
        { init = {}
        , update = update
        , view = view
        }
