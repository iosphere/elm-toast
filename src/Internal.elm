module Internal exposing (..)

import Types exposing (..)


updateNotifications : Toast a -> List (Notification a) -> Toast a
updateNotifications (Toast toast) list =
    Toast { toast | notifications = list }


config : Toast a -> InternalConfig
config (Toast toast) =
    toast.config


listAllNotifications : Toast a -> List (Notification a)
listAllNotifications (Toast toast) =
    toast.notifications


sanityCheck : Notification a -> Bool
sanityCheck (Notification notification) =
    notification.startTime < notification.expirationTime
