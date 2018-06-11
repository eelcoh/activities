module Authentication exposing (..)

import Types exposing (Model, Msg(..), Credentials(..), Token(..))
import RemoteData.Http as Web
import RemoteData exposing (RemoteData(..), WebData)
import Json.Encode
import Json.Decode exposing (Decoder, andThen, maybe, field)
import Element exposing (column, row)
import Element.Input as Input
import Element.Events as Events
import Element.Attributes exposing (px, padding, paddingLeft, paddingTop, paddingBottom, paddingXY, spacing, alignLeft, verticalSpread, center, alignRight, width, height)
import UI.Style
import UI.Button


authenticate : String -> String -> Cmd Msg
authenticate uid pw =
    let
        credentials =
            encodeCredentials uid pw
    in
        Web.post "/authentications/" FetchedToken decode credentials


viewLoginForm : Model -> Element.Element UI.Style.Style variation Msg
viewLoginForm model =
    let
        placeholder p =
            Input.placeholder { text = p, label = Input.hiddenLabel p }

        username v =
            let
                area =
                    { onChange = (\val -> SetUsername val)
                    , value = v
                    , label = placeholder "Username"
                    , options = []
                    }
            in
                Input.multiline UI.Style.TextInput [ Events.onFocus ShowCommentInput, height (px 36) ] area

        password v =
            let
                area =
                    { onChange = (\val -> SetPassword val)
                    , value = v
                    , label = placeholder "Password"
                    , options = []
                    }
            in
                Input.text UI.Style.TextInput [ height (px 36) ] area

        loginButton isSubmittable =
            if isSubmittable then
                UI.Button.pill UI.Style.Active Authenticate "login!"
            else
                UI.Button.pill UI.Style.Inactive None "je moet beide velden invullen"

        ( inpUsername, inpPassword, isSubmittable ) =
            case model.credentials of
                Types.Empty ->
                    ( username "", password "", False )

                WithPassword pw ->
                    ( username "", password pw, False )

                WithUsername uid ->
                    ( username uid, password "", False )

                Submittable uid pw ->
                    ( username uid, password pw, True )
    in
        Element.column UI.Style.CommentInputBox
            [ padding 10, spacing 20 ]
            [ inpUsername
            , inpPassword
            , loginButton isSubmittable
            ]


isAuthorised : Model -> Bool
isAuthorised model =
    case model.token of
        Success _ ->
            True

        _ ->
            False



-- Json


encodeCredentials : String -> String -> Json.Encode.Value
encodeCredentials uid pw =
    Json.Encode.object
        [ ( "username", Json.Encode.string uid )
        , ( "password", Json.Encode.string pw )
        ]


decode : Decoder Token
decode =
    (field "token" Json.Decode.string)
        |> Json.Decode.map
            Token
