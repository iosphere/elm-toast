module Minimal exposing (..)

import Html exposing (Html)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Time exposing (Time)
import Toast exposing (Toast)


-- MODEL


type alias Model =
    { time : Time
    , toast : Toast String
    }


init : ( Model, Cmd Msg )
init =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { -- Ideally your implementation should start with the correct time.
      -- To avoid flags we start with 0 and wait for the first update.
      time = 0
    , -- initialize toast
      toast = Toast.init
    }



-- UPDATE


type Msg
    = Tick Time
    | Postnotification String


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
                newToastNotification =
                    Toast.createNotification notification (model.time + Time.second * 3)

                newToast =
                    Toast.addNotification newToastNotification model.toast
            in
                updateToast newToast model


updateToast : Toast String -> Model -> Model
updateToast toast model =
    { model | toast = toast }


updateTime : Time -> Model -> Model
updateTime time model =
    { model | time = time }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every (Time.second * 1) Tick



-- VIEW


view : Model -> Html Msg
view model =
    Html.div []
        (boilerPlateViews
            ++ [ notificationsView model ]
        )


notificationsView : Model -> Html Msg
notificationsView model =
    Html.ul [ class "notifications-list" ] (Toast.views model.toast model.time notificationView)


notificationView : Toast.NotificationState -> String -> Html Msg
notificationView state notification =
    Html.li [] [ Html.text notification ]



-- BOILERPLATE


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


boilerPlateViews : List (Html Msg)
boilerPlateViews =
    [ Html.button [ onClick (Postnotification "My Message") ] [ Html.text "Post simple notification" ]
    , Html.h1 [] [ Html.text "Notifications" ]
    ]
