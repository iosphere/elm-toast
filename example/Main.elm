module Main exposing (..)

import Html exposing (Html)
import Html.Events exposing (onClick)
import Time exposing (Time, second)
import Toast exposing (Toast)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



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
    { time = 0, toast = Toast.init }



-- UPDATE


type Msg
    = Tick Time
    | Postnotification String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            let
                newToast =
                    Toast.update newTime model.toast
            in
                (updateTime newTime model
                    |> updateToast newToast
                )
                    ! []

        Postnotification string ->
            let
                newNotification =
                    Toast.createNotification string startTime expirationTime

                startTime =
                    -- Show immediately
                    0

                expirationTime =
                    model.time + Time.second * 5

                newToast =
                    Toast.addNotification newNotification model.toast
            in
                (updateToast newToast model) ! []


updateToast : Toast String -> Model -> Model
updateToast toast model =
    { model | toast = toast }


updateTime : Time -> Model -> Model
updateTime time model =
    { model | time = time }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every second Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        modelText =
            toString model
                |> Html.text
    in
        Html.div []
            [ Html.button [ onClick (Postnotification "Hello") ] [ Html.text "Post notification" ]
            , Html.div []
                [ Html.h1 [] [ Html.text "Notifications" ]
                , Html.ul [] (Toast.views model.toast model.time notificationView)
                ]
            ]


notificationView : Toast.NotificationState -> String -> Html Msg
notificationView state string =
    Html.li [] [ Html.text string ]
