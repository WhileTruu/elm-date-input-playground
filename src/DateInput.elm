module DateInput exposing (..)

import DateInput.Format exposing (Format, YearMonthDay)
import DateInput.Internal as Internal
import Html.Styled as Html exposing (Html)



-- STATE


type State
    = State Internal.State


init : State
init =
    State Internal.init



-- VIEW


view : { format : Format YearMonthDay } -> (State -> msg) -> State -> Html msg
view { format } onChange (State state) =
    Internal.view { format = format } (onChange << State) state
