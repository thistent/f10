module Delay exposing (Delay, dec, orig, ready, reset, switch, timer, update, wait)


type Delay a
    = Wait Float Float (Maybe a)
    | Ready Float a


wait : Float -> Maybe a -> Delay a
wait time content =
    Wait time time content


ready : Delay a -> a -> Delay a
ready delay content =
    case delay of
        Wait o _ _ ->
            Ready o content

        Ready o _ ->
            Ready o content


dec : Float -> Delay a -> Delay a
dec delta delay =
    case delay of
        Wait o t content ->
            let
                newTime =
                    t - delta
            in
            if newTime > 0 then
                Wait o newTime content

            else
                case content of
                    Just c ->
                        Ready o c

                    Nothing ->
                        Wait o 0 Nothing

        Ready o c ->
            Ready o c


switch : a -> a -> Delay b -> a
switch notReadyState readyState delay =
    case delay of
        Ready _ _ ->
            readyState

        _ ->
            notReadyState


timer : Delay b -> Maybe Float
timer delay =
    case delay of
        Wait _ time _ ->
            Just time

        Ready _ _ ->
            Nothing


orig : Delay b -> Float
orig delay =
    case delay of
        Wait o _ _ ->
            o

        Ready o _ ->
            o


reset : Delay b -> Delay b
reset delay =
    case delay of
        Wait o t mc ->
            Wait o o mc

        Ready o c ->
            Wait o o (Just c)


update : Delay b -> b -> Delay b
update delay content =
    case delay of
        Wait o t _ ->
            Wait o t (Just content)

        Ready o _ ->
            Ready o content
