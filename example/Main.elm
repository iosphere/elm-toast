module Main exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Html.Keyed
import Time exposing (Time)
import Toast exposing (Toast)


-- MODEL


type alias Model =
    { time : Time
    , toast : Toast AppNotification
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { -- Ideally your implementation should start with the correct time.
      -- To avoid flags we start with 0 and wait for the first update.
      time = 0
    , -- initialize toast with a delay of 1.5 seconds. This is the duration
      -- for which notifications will remain "active" and in the DOM after their
      -- expiration. Their state will be `Toast.Hiding` and will give the css
      -- transition enough time to complete before removing the DOM element.
      toast = Toast.initWithTransitionDelay (Time.second * 1.5)
    }


type alias AppNotification =
    { message : String
    , important : Bool
    }



-- UPDATE


type Msg
    = Tick Time
    | Postnotification AppNotification


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    pureUpdate msg model ! []


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

        Postnotification notification ->
            let
                startTime =
                    -- Show immediately
                    0

                expirationTime =
                    (model.time + Time.second * 3)

                newToastNotification =
                    Toast.createFutureNotification startTime notification expirationTime

                newToast =
                    Toast.addNotification newToastNotification model.toast
            in
                updateToast newToast model


updateToast : Toast AppNotification -> Model -> Model
updateToast toast model =
    { model | toast = toast }


updateTime : Time -> Model -> Model
updateTime time model =
    { model | time = time }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (Time.millisecond * 500) Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        modelText =
            toString model
                |> Html.text
    in
        Html.div []
            (boilerPlateView
                ++ [ notificationsView model ]
            )


notificationsView : Model -> Html Msg
notificationsView model =
    Html.Keyed.ul [ class "notifications-list" ] (Toast.keyedViews model.toast model.time notificationView)


notificationView : Toast.NotificationState -> AppNotification -> Html Msg
notificationView state notification =
    Html.li
        [ classList
            [ ( "notification", True )
            , ( "notification--important", notification.important )
            , ( "notification--hidden", (state == Toast.Hiding) )
            ]
        ]
        [ Html.div [ class "notification__content" ] [ Html.text (notification.message ++ " | " ++ toString state) ] ]



-- BOILERPLATE


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


boilerPlateView : List (Html Msg)
boilerPlateView =
    [ Html.node "link" [ Html.Attributes.rel "stylesheet", Html.Attributes.href "style.css" ] []
    , Html.button [ onClick (Postnotification (AppNotification "This is important" True)) ] [ Html.text "Post import notification" ]
    , Html.button [ onClick (Postnotification (AppNotification "Not so important" False)) ] [ Html.text "Post normal notification" ]
    , Html.h1 [] [ Html.text "Notifications" ]
    ]
