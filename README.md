# elm-toast

A view agnostic way to handle toasts and other temporary notifications.

A typical application would be to display temporary notifications as Html views
in your elm app. This module takes care of updating the notifications and let's
you decide what content should be included in your notifications and how these
should be represented in your view.

A [demo](https://ellie-app.com/3rkbwjFF6KBa1) can be found here: https://ellie-app.com/3rkbwjFF6KBa1

# Screenshot

This is a screenshot of the [example app](example/Main.elm). The design is part
of your app's responsibility. The concept of this package is that it does not
impose any view type or design on your app (it does not even require views
to be Html).

![Screenshot of elm-toast example](https://github.com/iosphere/elm-toast/raw/master/screenshot.png)

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
