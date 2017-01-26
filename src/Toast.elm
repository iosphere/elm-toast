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

{-| A view agnostic way to handle toasts and other temporary notifications.


# Setup

You are responsible to keep the Toast somewhere in your app model. And to wire
up a Time Tick subscription to regularly update it.

@docs init, initWithTransitionDelay

# Create, add and list notifications

@docs addNotification, createNotification, listActiveNotifications

# Update

@docs update

# Create views from the current Toast

@docs views

# Types

@docs NotificationState


# Opaque types
@docs Toast, Notification
-}

import Time exposing (Time)
import Types
import Internal


{-| -}
type alias Toast a =
    Types.Toast a


{-| -}
type alias Notification a =
    Types.Notification a


{-| -}
type NotificationState
    = Visible
    | Hiding


{-| -}
init : Toast a
init =
    initWithTransitionDelay 0


{-| -}
initWithTransitionDelay : Float -> Toast a
initWithTransitionDelay delay =
    Types.Toast (Types.InternalToast (Types.InternalConfig delay) [])


{-| -}
createNotification : a -> Time -> Time -> Notification a
createNotification value start expiration =
    Types.Notification
        { startTime = start
        , expirationTime = expiration
        , message = value
        }


{-| -}
addNotification : Notification a -> Toast a -> Toast a
addNotification notification toast =
    if Internal.sanityCheck notification then
        (Internal.listAllNotifications toast)
            ++ [ notification ]
            |> Internal.updateNotifications toast
    else
        toast


{-| -}
listActiveNotifications : Toast a -> Time -> List (Notification a)
listActiveNotifications toast time =
    case toast of
        Types.Toast internalToast ->
            filterActiveNotifications time internalToast.config internalToast.notifications


{-| -}
views : Toast a -> Time -> (NotificationState -> a -> viewType) -> List viewType
views toast time viewFunc =
    let
        config =
            Internal.config toast
    in
        Internal.listAllNotifications toast
            |> List.filterMap
                (\notification ->
                    case notification of
                        Types.Notification internalNotification ->
                            case notificationState time config notification of
                                Active externalState ->
                                    viewFunc externalState internalNotification.message
                                        |> Just

                                Inactive ->
                                    Nothing

                                Past ->
                                    Nothing
                )



-- Update


{-| -}
update : Time -> Toast a -> Toast a
update time toast =
    case toast of
        Types.Toast internalToast ->
            Internal.listAllNotifications toast
                |> filterValidNotifications time internalToast.config
                |> Internal.updateNotifications toast



-- Internal methods using external union types


type InternalNotificationState
    = Active NotificationState
    | Inactive
    | Past


notificationState : Time -> Types.InternalConfig -> Notification a -> InternalNotificationState
notificationState time config notification =
    case Debug.log "checkingState for" notification of
        Types.Notification internalNotification ->
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


filterValidNotifications : Time -> Types.InternalConfig -> List (Notification a) -> List (Notification a)
filterValidNotifications time config =
    List.filter (\note -> (notificationState time config note) /= Past)


filterActiveNotifications : Time -> Types.InternalConfig -> List (Notification a) -> List (Notification a)
filterActiveNotifications time config =
    List.filter
        (\note ->
            case notificationState time config note of
                Active _ ->
                    True

                _ ->
                    False
        )
