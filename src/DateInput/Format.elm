module DateInput.Format exposing
    ( Format
    , Token(..)
    , YearMonthDay
    , day
    , format
    , month
    , text
    , toString
    , toTokens
    , tokenToString
    , year
    )


type Format a
    = Format (List Token)


type Token
    = TokenMonth
    | TokenDay
    | TokenYear
    | TokenText String


type alias YearMonthDay =
    { year : (), month : (), day : () }


format : Format { monthCompatible : (), dayCompatible : (), yearCompatible : () }
format =
    Format []


year : Format { a | yearCompatible : () } -> Format { a | year : () }
year (Format tokens) =
    Format (TokenYear :: tokens)


month : Format { a | monthCompatible : () } -> Format { a | month : () }
month (Format tokens) =
    Format (TokenMonth :: tokens)


day : Format { a | dayCompatible : () } -> Format { a | day : () }
day (Format tokens) =
    Format (TokenDay :: tokens)


text : String -> Format a -> Format a
text string (Format tokens) =
    Format (TokenText string :: tokens)


toString : Format a -> String
toString (Format tokens) =
    List.map tokenToString tokens
        |> List.reverse
        |> String.join ""


tokenToString : Token -> String
tokenToString token =
    case token of
        TokenMonth ->
            "mm"

        TokenDay ->
            "dd"

        TokenYear ->
            "yyyy"

        TokenText string ->
            string


toTokens : Format YearMonthDay -> List Token
toTokens (Format tokens) =
    List.reverse tokens
