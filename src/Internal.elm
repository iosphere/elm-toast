module Internal exposing (..)

import Types exposing (..)


updateNotifications : Toast a -> List (Notification a) -> Toast a
updateNotifications toast list =
    case toast of
        Toast internalToast ->
            Toast
                { internalToast | notifications = list }


config : Toast a -> InternalConfig
config toast =
    case toast of
        Toast internalToast ->
            internalToast.config


listAllNotifications : Toast a -> List (Notification a)
listAllNotifications toast =
    case toast of
        Toast internalToast ->
            internalToast.notifications


sanityCheck : Notification a -> Bool
sanityCheck notification =
    case notification of
        Notification internalNotification ->
            internalNotification.startTime < internalNotification.expirationTime
