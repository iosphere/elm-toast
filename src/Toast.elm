module Toast
    exposing
        ( Duration
        , Notification
        , NotificationState(..)
        , Timestamp
        , Toast
        , ViewFunction
        , addNotification
        , createFutureNotification
        , createNotification
        , init
        , initWithTransitionDelay
        , keyedViews
        , listActiveNotifications
        , updateTimestamp
        , views
        )

{-| A view agnostic way to handle toasts and other temporary notifications.

A typical application would be to display temporary notifications as Html views
in your elm app. This module takes care of updating the notifications and let's
you decide what content should be included in your notifications and how these
should be represented in your view.

* [Minimal implementation](#minimal-implementation)
* [Reference](#reference)
    * [Setup](#setup)
    * [Create, add and list notifications](#create-add-and-list-notifications)
    * [Update](#update)
    * [Create views](#create-views-from-the-current-toast)
    * [Types](#types)
    * [Opaque types](#opaque-types)

# Minimal implementation

Your app should contain the current time and the Toast somewhere in its model.

    type alias Model =
        { time : Time
        , toast : Toast String
        }

You will then need to subscribe to time updates. The frequency of the updates
will determine the update frequency of the toast and how accurately start times,
expiration times and transition delays are respected:

    type Msg
        = Tick Time
        | PostNotification String


    subscriptions : Model -> Sub Msg
    subscriptions model =
        Time.every (Time.second * 1) Tick

In your update method you will need to update the time and update the toast in
your model on every `Tick` message. In addition you will probably want to add
any posted notification to your model's `Toast`:

    pureUpdate : Msg -> Model -> Model
    pureUpdate msg model =
        case msg of
            Tick newTime ->
                let
                    newToast =
                        Toast.updateTimestamp newTime model.toast
                in
                    updateTime newTime model
                        |> updateToast newToast

            PostNotification notification ->
                let
                    newToastNotification =
                        Toast.createNotification notification (model.time + Time.second * 3)

                    newToast =
                        Toast.addNotification newToastNotification model.toast
                in
                    updateToast newToast model


    updateTime : Time -> Model -> Model
    updateTime time model =
        { model | time = time }


    updateToast : Toast String -> Model -> Model
    updateToast toast model =
        { model | toast = toast }


# Reference

## Setup

You are responsible to keep the `Toast` somewhere in your app model.
You will also need to wire up a `Time` subscription to regularly update it
(see [Update](#update) and[Minimal implementation](#minimal-implementation)).

@docs init, initWithTransitionDelay

## Create, add and list notifications

@docs addNotification, createNotification, createFutureNotification, listActiveNotifications

## Update

@docs updateTimestamp

## Create views from the current Toast

@docs ViewFunction, views, keyedViews

## Types

@docs NotificationState, Timestamp, Duration

## Opaque types

@docs Toast, Notification

-}

import Time exposing (Time)
import Types
import Internal


{-| Opaque type encapsulating all active notifications. Use `init` or
`initWithTransitionDelay` to create a Toast.
-}
type alias Toast a =
    Types.Toast a


{-| Type alias to identify `Time` parameters used as timestamps.
-}
type alias Timestamp =
    Time


{-| Type alias to identify `Time` parameters used as duration.
-}
type alias Duration =
    Time


{-| -}
type alias Notification a =
    Types.Notification a


{-| When using a Toast without delay the state of a notification handed to you
will always be visible. Hidden notifications will never be made available to
you.

If you use a Toast with a transition delay, the state will be `Hiding` once the
expiration time has been reached until the transition delay has passed.

You can use this information in your views to set the view to hidden and apply
a transition.
-}
type NotificationState
    = Visible
    | Hiding


{-| Initialize a Toast without transition delay.

Any notification will be removed from the list of active notifications as soon
as the expiration time is reached.

The `notificationType` will determine what values are made available to your
view function. The rest of the documentation will simply refer to this type as
`a`.
-}
init : Toast notificationType
init =
    initWithTransitionDelay 0


{-| Initialize a Toast with a transition delay.

This initializer is useful if you want to remove notifications from your view
with a transition. For the sake of simplicity we assume that you use Html
as the view type.

The transition delay is the duration for which notifications will remain
"active" and in the DOM after their expiration. Their state will be
`Hiding` (see `NotificationState`) and will give the css transition enough time to complete
before removing the DOM element.

Make sure to use `Time.second` (or other constructors)
to provide a meaningful parameter.
-}
initWithTransitionDelay : Duration -> Toast a
initWithTransitionDelay delay =
    Types.Toast (Types.InternalToast (Types.InternalConfig delay) [])


{-| Create a new Notification with type a. The type of the notification will
need to match the type of the Toast in your app model when adding it via
`addNotification`.

The time parameter is the expiration time after which the notification should
no longer remain in the `Visible` state. The exact time at which the
notification changes its state will depend on the frequency with which
you call `update` (e.g. if you subscribe to update every second the expiration
time will be respected up to a one second precision).
-}
createNotification : a -> Timestamp -> Notification a
createNotification notificationValue expiration =
    createFutureNotification 0 notificationValue expiration


{-| Create a notification that will be shown at some point in the future.

The first parameter is the start time at which the notification should start to
be Visible. See [`createNotification`](#createNotification) for the remaining
parameters.

If you create a notification with a start time after its expiration time it will
be discarded when adding it to the Toast.
-}
createFutureNotification : Timestamp -> a -> Timestamp -> Notification a
createFutureNotification start notificationValue expiration =
    Types.Notification
        { startTime = start
        , expirationTime = expiration
        , message = notificationValue
        }


{-| Add a previously created notification to the Toast.
Duplicate notifications will be discarded.
-}
addNotification : Notification a -> Toast a -> Toast a
addNotification notification toast =
    if
        Internal.sanityCheck notification
            && not (Internal.member notification toast)
    then
        (Internal.listAllNotifications toast)
            ++ [ notification ]
            |> Internal.updateNotifications toast
    else
        toast


{-| Get a list of all active notifications in the toast. This excludes any
hidden notifications. You will need to pass in your model's current timestamp.
-}
listActiveNotifications : Toast a -> Timestamp -> List (Notification a)
listActiveNotifications (Types.Toast toast) time =
    filterActiveNotifications time toast.config toast.notifications



-- Views


{-| A function that creates some view representation (e.g. Html) from a
notification type `a` as used in your Toast.
-}
type alias ViewFunction a viewType =
    NotificationState -> a -> viewType


{-| Create a list of views from the current list of active notifications in the
given Toast at the provided timestamp.

The view function you provided will be called for each active notification value
(of type `a`) and its state to provide some view representation of type
`viewType` (e.g. Html).
-}
views : Toast a -> Timestamp -> ViewFunction a viewType -> List viewType
views toast time viewFunc =
    let
        config =
            Internal.config toast
    in
        Internal.listAllNotifications toast
            |> List.filterMap (view config time viewFunc)


{-| Similar to the `views` function, but providing the string
representation of the notification along with the view in a Tuple,
which makes it easy to use with Html.Keyed.
-}
keyedViews : Toast a -> Timestamp -> ViewFunction a viewType -> List ( String, viewType )
keyedViews toast time viewFunc =
    let
        config =
            Internal.config toast
    in
        Internal.listAllNotifications toast
            |> List.filterMap
                (\notification ->
                    view config time viewFunc notification
                        |> Maybe.map (\view -> ( toString notification, view ))
                )



-- Update


{-| With every time update message you will need to update the Toast by calling
this funtion with the current time and then update the Toast in your model
with the returned value:

    pureUpdate : Msg -> Model -> Model
    pureUpdate msg model =
        case msg of
            Tick newTime ->
                let
                    newToast =
                        Toast.updateTimestamp newTime model.toast
                in
                    { model | toast = newToast, time = newTime }

-}
updateTimestamp : Timestamp -> Toast a -> Toast a
updateTimestamp time ((Types.Toast internalToast) as toast) =
    Internal.listAllNotifications toast
        |> filterValidNotifications time internalToast.config
        |> Internal.updateNotifications toast



-- Internal methods using external union types
{-
   The following method rely on the external union type NotificationState are
   included in this module to avoid circular imports.
-}


type InternalNotificationState
    = Active NotificationState
    | Inactive
    | Past


notificationState : Timestamp -> Types.InternalConfig -> Notification a -> InternalNotificationState
notificationState time config notification =
    case notification of
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


filterValidNotifications : Timestamp -> Types.InternalConfig -> List (Notification a) -> List (Notification a)
filterValidNotifications time config =
    List.filter (\note -> (notificationState time config note) /= Past)


filterActiveNotifications : Timestamp -> Types.InternalConfig -> List (Notification a) -> List (Notification a)
filterActiveNotifications time config =
    List.filter
        (\note ->
            case notificationState time config note of
                Active _ ->
                    True

                _ ->
                    False
        )


view : Types.InternalConfig -> Timestamp -> ViewFunction a viewType -> Types.Notification a -> Maybe viewType
view config time viewFunc ((Types.Notification internalNotification) as notification) =
    case notificationState time config notification of
        Active externalState ->
            viewFunc externalState internalNotification.message
                |> Just

        Inactive ->
            Nothing

        Past ->
            Nothing
