module Types exposing (..)

import Time exposing (Time)


type Toast a
    = Toast (InternalToast a)


type Notification a
    = Notification (InternalNotification a)


type alias InternalConfig =
    { hideTransitionDelay : Float
    }


type alias InternalToast a =
    { config : InternalConfig
    , notifications : List (Notification a)
    }


type alias InternalNotification a =
    { startTime : Time
    , expirationTime : Time
    , message : a
    }
