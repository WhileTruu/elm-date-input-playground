module Main exposing (main)

import Browser
import Css
import DateInput
import DateInput.Format
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs


type alias Model =
    { count : Int
    , dateInputState : DateInput.State
    }


initialModel : Model
initialModel =
    { count = 0, dateInputState = DateInput.init }


type Msg
    = Increment
    | Decrement
    | DateInputChanged DateInput.State


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | count = model.count + 1 }

        Decrement ->
            { model | count = model.count - 1 }

        DateInputChanged state ->
            { model | dateInputState = state }


view : Model -> Html Msg
view model =
    let
        format : DateInput.Format.Format DateInput.Format.YearMonthDay
        format =
            DateInput.Format.format
                |> DateInput.Format.day
                |> DateInput.Format.text "."
                |> DateInput.Format.month
                |> DateInput.Format.text "."
                |> DateInput.Format.year
    in
    Html.div
        [ Attrs.css
            [ Css.margin Css.auto
            , Css.width (Css.px 500)
            , Css.marginTop (Css.px 100)
            ]
        ]
        [ Html.h1 [] [ Html.text "Date inputs and stuff" ]
        , Html.h2 [] [ Html.text "The html one" ]
        , Html.div []
            [ Html.input [ Attrs.type_ "date" ] [] ]
        , Html.h2 [] [ Html.text "The component one" ]
        , DateInput.view
            { format = format
            }
            DateInputChanged
            model.dateInputState
        ]


main : Program () Model Msg
main =
    Browser.sandbox
        { init = initialModel
        , view = Html.toUnstyled << view
        , update = update
        }
