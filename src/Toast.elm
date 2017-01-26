module Toast
    exposing
        ( Toast
        , Notification
        , NotificationState
        , addNotification
        , createNotification
        , init
        , initWithTransitionDelay
        , listActiveNotifications
        , update
        , views
        )

import Time exposing (Time)


type Toast a
    = Toast (InternalToast a)


type Notification a
    = Notification (InternalNotification a)


type NotificationState
    = Visible
    | Hiding


type alias InternalConfig =
    { hideTransitionDelay : Float
    }


type InternalNotificationState
    = Active NotificationState
    | Inactive
    | Past


type alias InternalToast a =
    { config : InternalConfig
    , notifications : List (Notification a)
    }


type alias InternalNotification a =
    { startTime : Time
    , expirationTime : Time
    , message : a
    }


init : Toast a
init =
    initWithTransitionDelay 0


initWithTransitionDelay : Float -> Toast a
initWithTransitionDelay delay =
    Toast (InternalToast (InternalConfig delay) [])


createNotification : a -> Time -> Time -> Notification a
createNotification value start expiration =
    Notification
        { startTime = start
        , expirationTime = expiration
        , message = value
        }


addNotification : Notification a -> Toast a -> Toast a
addNotification notification toast =
    if sanityCheck notification then
        (listAllNotifications toast)
            ++ [ notification ]
            |> internalUpdateNotifications toast
    else
        toast


listActiveNotifications : Toast a -> Time -> List (Notification a)
listActiveNotifications toast time =
    case toast of
        Toast internalToast ->
            filterActiveNotifications time internalToast.config internalToast.notifications


listAllNotifications : Toast a -> List (Notification a)
listAllNotifications toast =
    case toast of
        Toast internalToast ->
            internalToast.notifications


internalConfig : Toast a -> InternalConfig
internalConfig toast =
    case toast of
        Toast internalToast ->
            internalToast.config


views : Toast a -> Time -> (NotificationState -> a -> viewType) -> List viewType
views toast time viewFunc =
    let
        config =
            internalConfig toast
    in
        listAllNotifications toast
            |> List.filterMap
                (\notification ->
                    case notification of
                        Notification internalNotification ->
                            case notificationState time config notification of
                                Active externalState ->
                                    viewFunc externalState internalNotification.message
                                        |> Just

                                Inactive ->
                                    Nothing

                                Past ->
                                    Nothing
                )


internalUpdateNotifications : Toast a -> List (Notification a) -> Toast a
internalUpdateNotifications toast list =
    case toast of
        Toast internalToast ->
            Toast
                { internalToast | notifications = list }



-- Update


update : Time -> Toast a -> Toast a
update time toast =
    case toast of
        Toast internalToast ->
            listAllNotifications toast
                |> filterValidNotifications time internalToast.config
                |> internalUpdateNotifications toast


filterValidNotifications : Time -> InternalConfig -> List (Notification a) -> List (Notification a)
filterValidNotifications time config =
    List.filter (\note -> (notificationState time config note) /= Past)


filterActiveNotifications : Time -> InternalConfig -> List (Notification a) -> List (Notification a)
filterActiveNotifications time config =
    List.filter
        (\note ->
            case notificationState time config note of
                Active _ ->
                    True

                _ ->
                    False
        )


notificationState : Time -> InternalConfig -> Notification a -> InternalNotificationState
notificationState time config notification =
    case Debug.log "checkingState for" notification of
        Notification internalNotification ->
            if internalNotification.startTime > time then
                -- Future notification
                Inactive
            else if internalNotification.expirationTime > time then
                -- Visible notification expiration in the future
                Active Visible
            else if internalNotification.expirationTime + config.hideTransitionDelay >= time then
                -- Currently dismissing
                Active Hiding
            else
                -- Past
                Past


sanityCheck : Notification a -> Bool
sanityCheck notification =
    case notification of
        Notification internalNotification ->
            internalNotification.startTime < internalNotification.expirationTime
