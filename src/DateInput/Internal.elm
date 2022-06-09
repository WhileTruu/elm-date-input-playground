module DateInput.Internal exposing (..)

import Css
import DateInput.Format as Format exposing (Format, YearMonthDay)
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attrs
import Html.Styled.Events as Events
import Json.Decode as JD
import List.Extra



-- STATE


type alias State =
    { day : String
    , month : String
    , year : String
    , hilight : Hilight
    , showHilights : Bool
    }


init : State
init =
    { day = ""
    , month = ""
    , year = ""
    , hilight = HilightDay
    , showHilights = False
    }


type Hilight
    = HilightDay
    | HilightMonth
    | HilightYear


hilightFromToken : Format.Token -> Maybe Hilight
hilightFromToken token =
    case token of
        Format.TokenDay ->
            Just HilightDay

        Format.TokenMonth ->
            Just HilightMonth

        Format.TokenYear ->
            Just HilightYear

        Format.TokenText _ ->
            Nothing


nextHilight : Hilight -> List Format.Token -> Hilight
nextHilight hilight tokens =
    case tokens of
        [] ->
            hilight

        a :: others ->
            if Just hilight == hilightFromToken a then
                firstHilightable others
                    |> Maybe.withDefault hilight

            else
                nextHilight hilight (others ++ [ a ])


firstHilightable : List Format.Token -> Maybe Hilight
firstHilightable tokens =
    case tokens of
        [] ->
            Nothing

        x :: xs ->
            case hilightFromToken x of
                Just hilight ->
                    Just hilight

                Nothing ->
                    firstHilightable xs


updateDigitIn : { format : Format YearMonthDay } -> State -> Char -> State
updateDigitIn { format } state digit =
    let
        tokens : List Format.Token
        tokens =
            Format.toTokens format
    in
    case state.hilight of
        HilightDay ->
            let
                day : String
                day =
                    if String.length state.day < 2 then
                        state.day ++ String.fromChar digit

                    else
                        String.fromChar digit
            in
            { state
                | day = day
                , hilight =
                    if String.length day == 2 && firstHilightable (List.reverse tokens) /= Just HilightDay then
                        nextHilight state.hilight tokens

                    else
                        state.hilight
            }

        HilightMonth ->
            let
                month : String
                month =
                    if String.length state.month < 2 then
                        state.month ++ String.fromChar digit

                    else
                        String.fromChar digit
            in
            { state
                | month = month
                , hilight =
                    if String.length month == 2 && firstHilightable (List.reverse tokens) /= Just HilightMonth then
                        nextHilight state.hilight tokens

                    else
                        state.hilight
            }

        HilightYear ->
            let
                year : String
                year =
                    if String.length state.year < 4 then
                        state.year ++ String.fromChar digit

                    else
                        String.fromChar digit
            in
            { state
                | year = year
                , hilight =
                    if String.length year == 4 && firstHilightable (List.reverse tokens) /= Just HilightYear then
                        nextHilight state.hilight tokens

                    else
                        state.hilight
            }


addDigit : { digits : Int } -> Int -> Int -> Int
addDigit { digits } digit value =
    if value // (10 ^ (digits - 1)) >= 1 then
        digit

    else
        10 * value + digit



-- VIEW


view : { format : Format YearMonthDay } -> (State -> msg) -> State -> Html msg
view { format } onChange state =
    let
        tokens : List Format.Token
        tokens =
            Format.toTokens format
    in
    Html.div
        [ Attrs.class "dateinput-input"
        , Attrs.css
            [ Css.display Css.inlineFlex
            , Css.border2 (Css.px 2) Css.solid
            , Css.padding (Css.px 5)
            ]
        , hijackOnKeyDown
            (List.concat
                [ if firstHilightable tokens /= Just state.hilight then
                    [ tabBack (onChange { state | hilight = nextHilight state.hilight (List.reverse tokens) }) ]

                  else
                    []
                , if firstHilightable (List.reverse tokens) /= Just state.hilight then
                    [ tab (onChange { state | hilight = nextHilight state.hilight tokens }) ]

                  else
                    []
                , [ digitKeyHandler (onChange << updateDigitIn { format = format } state)
                  ]
                ]
            )
        , Events.onFocus
            (onChange
                { state
                    | hilight = Maybe.withDefault state.hilight (firstHilightable tokens)
                    , showHilights = True
                }
            )
        , Events.onBlur
            (onChange
                { state
                    | hilight = HilightMonth
                    , showHilights = False
                    , year =
                        state.year
                            |> String.toInt
                            |> Maybe.map
                                (\year ->
                                    if year <= 24 then
                                        -- FIXME something like larger than current year + 2?
                                        2000 + year

                                    else if year < 99 then
                                        1900 + year

                                    else
                                        year
                                )
                            |> Maybe.map String.fromInt
                            |> Maybe.withDefault state.year
                }
            )
        , Attrs.tabindex 0
        ]
        (List.map
            (\a ->
                case a of
                    Format.TokenDay ->
                        Html.span
                            [ Attrs.css
                                [ if state.hilight == HilightDay && state.showHilights then
                                    Css.backgroundColor (Css.hex "f00")

                                  else
                                    Css.batch []
                                ]
                            ]
                            [ Html.text
                                (if not (String.isEmpty state.day) then
                                    String.padLeft 2 '0' state.day

                                 else
                                    Format.tokenToString Format.TokenDay
                                )
                            ]

                    Format.TokenMonth ->
                        Html.span
                            [ Attrs.css
                                [ if state.hilight == HilightMonth && state.showHilights then
                                    Css.backgroundColor (Css.hex "f00")

                                  else
                                    Css.batch []
                                ]
                            ]
                            [ Html.text
                                (if not (String.isEmpty state.month) then
                                    String.padLeft 2 '0' state.month

                                 else
                                    Format.tokenToString Format.TokenMonth
                                )
                            ]

                    Format.TokenYear ->
                        Html.span
                            [ Attrs.css
                                [ if state.hilight == HilightYear && state.showHilights then
                                    Css.backgroundColor (Css.hex "f00")

                                  else
                                    Css.batch []
                                ]
                            ]
                            [ Html.text
                                (if not (String.isEmpty state.year) then
                                    String.padLeft 4 '0' state.year

                                 else
                                    Format.tokenToString Format.TokenYear
                                )
                            ]

                    Format.TokenText string ->
                        Html.span [ Attrs.css [ Css.whiteSpace Css.pre ] ] [ Html.text string ]
            )
            tokens
        )



-- HELPERS


hijackOn : String -> JD.Decoder msg -> Html.Attribute msg
hijackOn event msgDecoder =
    Events.preventDefaultOn event (JD.map (\msg -> ( msg, True )) msgDecoder)


tabKeyCode : Int
tabKeyCode =
    9


hijackOnKeyDown : List (KeyHandler msg) -> Html.Attribute msg
hijackOnKeyDown handlers =
    hijackOn "keydown"
        (JD.andThen
            (\{ keyCode, shiftKey } ->
                handlers
                    |> List.Extra.find
                        (\handler ->
                            handler.keyCode keyCode && handler.shiftKey == shiftKey
                        )
                    |> Maybe.map (JD.succeed << (\{ onEvent } -> onEvent keyCode))
                    |> Maybe.withDefault (JD.fail (String.fromInt keyCode))
            )
            (JD.map2
                (\keyCode shiftKey ->
                    { keyCode = keyCode, shiftKey = shiftKey }
                )
                Events.keyCode
                (JD.field "shiftKey" JD.bool)
            )
        )


type alias KeyHandler msg =
    { keyCode : Int -> Bool
    , shiftKey : Bool
    , onEvent : Int -> msg
    }


tab : msg -> KeyHandler msg
tab onEvent =
    { keyCode = (==) tabKeyCode, shiftKey = False, onEvent = \_ -> onEvent }


tabBack : msg -> KeyHandler msg
tabBack onEvent =
    { keyCode = (==) tabKeyCode, shiftKey = True, onEvent = \_ -> onEvent }


digitKeyHandler : (Char -> msg) -> KeyHandler msg
digitKeyHandler onEvent =
    { keyCode = \code -> code <= 0x39 && 0x30 <= code
    , shiftKey = False
    , onEvent = onEvent << Char.fromCode
    }
