module Tests exposing (all)

import Expect
import Test exposing (describe, test)
import Toast


all : Test.Test
all =
    describe "Toast"
        [ testActiveNotification, testViews ]


notificationValid : Toast.Notification String
notificationValid =
    Toast.createNotification "Valid" 0 110


notificationValidNow : Toast.Notification String
notificationValidNow =
    Toast.createNotification "Now" 75 100


notificationFuture : Toast.Notification String
notificationFuture =
    Toast.createNotification "Future" 200 300


notificationPast : Toast.Notification String
notificationPast =
    Toast.createNotification "Past" 0 50


toast : Float -> Toast.Toast String
toast delay =
    Toast.initWithTransitionDelay delay
        |> Toast.addNotification notificationValid
        |> Toast.addNotification notificationValidNow
        |> Toast.addNotification notificationFuture
        |> Toast.addNotification notificationPast


testActiveNotification : Test.Test
testActiveNotification =
    test "Test validNotifications" <|
        \() ->
            Toast.listActiveNotifications (toast 0) 75
                |> Expect.equal [ notificationValid, notificationValidNow ]


viewFunc : Toast.NotificationState -> String -> String
viewFunc state string =
    string ++ "|" ++ toString state


testViews : Test.Test
testViews =
    test "Test view" <|
        \() ->
            Toast.views (toast 5) 100 viewFunc
                |> Expect.equal [ "Valid|Visible", "Now|Hiding" ]
