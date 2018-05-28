module Main exposing (..)

import Html exposing (Html)
import Html exposing (Html, div, button, text, textarea, input, section)
import Element exposing (column, row)
import Element.Attributes exposing (px, percent, padding, paddingLeft, paddingTop, paddingBottom, paddingXY, spacing, alignLeft, verticalSpread, center, alignRight, width, height)
import UI.Style
import UI.Button
import UI.Text
import Navigation
import Uuid.Barebones as Uuid
import Types exposing (..)
import Activities
import Bets.Api exposing (retrieveBet)
import Bets.View
import RemoteData exposing (RemoteData(..), WebData)
import Window
import UI.Size as Size exposing (bodyWidth, classifyDevice)


newComment : Comment
newComment =
    { author = "", msg = "" }


newPost : Post
newPost =
    { author = "", title = "", msg = "", passphrase = "" }


newModel : Model
newModel =
    { activities = NotAsked
    , comment = newComment
    , post = newPost
    , contents = (div [] [ text "niks" ])
    , showComment = False
    , showPost = False
    , page = Home
    , bet = NotAsked
    , screenSize = Small
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update action model =
    case action of
        None ->
            ( model, Cmd.none )

        FetchedBet res ->
            ( { model | bet = res }, Cmd.none )

        FetchedActivities res ->
            ( { model | activities = res }, Cmd.none )

        SetCommentAuthor nwAuthor ->
            let
                oldComment =
                    model.comment

                nwComment =
                    { oldComment | author = nwAuthor }
            in
                ( { model | comment = nwComment }, Cmd.none )

        ShowCommentInput ->
            ( { model | showComment = True }, Cmd.none )

        HideCommentInput ->
            ( { model | showComment = False }, Cmd.none )

        SetCommentMsg nwMsg ->
            let
                oldComment =
                    model.comment

                nwComment =
                    { oldComment | msg = nwMsg }
            in
                ( { model | comment = nwComment }, Cmd.none )

        SaveComment ->
            let
                cmd =
                    Activities.saveComment model
            in
                ( model, cmd )

        SavedComment res ->
            ( { model | activities = res, comment = newComment, showComment = False }, Cmd.none )

        SetPostAuthor nwAuthor ->
            let
                oldPost =
                    model.post

                nwPost =
                    { oldPost | author = nwAuthor }
            in
                ( { model | post = nwPost }, Cmd.none )

        ShowPostInput ->
            ( { model | showPost = True }, Cmd.none )

        HidePostInput ->
            ( { model | showPost = False }, Cmd.none )

        SetPostMsg nwMsg ->
            let
                oldPost =
                    model.post

                nwPost =
                    { oldPost | msg = nwMsg }
            in
                ( { model | post = nwPost }, Cmd.none )

        SetPostTitle newTitle ->
            let
                oldPost =
                    model.post

                nwPost =
                    { oldPost | title = newTitle }
            in
                ( { model | post = nwPost }, Cmd.none )

        SetPostPassphrase nwPassphrase ->
            let
                oldPost =
                    model.post

                nwPost =
                    { oldPost | passphrase = nwPassphrase }
            in
                ( { model | post = nwPost }, Cmd.none )

        SavePost ->
            let
                cmd =
                    Activities.savePost model
            in
                ( model, cmd )

        SavedPost res ->
            ( { model | activities = res, post = newPost, showPost = False }, Cmd.none )

        UrlChange location ->
            let
                ( page, msg ) =
                    getPage location.hash

                newModel =
                    { model | page = page }
            in
                update msg newModel

        Click location ->
            ( model, Navigation.newUrl location )

        BetSelected ->
            let
                mUuid =
                    case model.page of
                        Bets uuid ->
                            Just uuid

                        _ ->
                            Nothing

                cmd =
                    case mUuid of
                        Just uuid ->
                            retrieveBet uuid FetchedBet

                        _ ->
                            Cmd.none
            in
                ( model, cmd )

        RefreshActivities ->
            ( model, Activities.fetchActivities model )

        SetScreenSize sz ->
            let
                screenSize =
                    classifyDevice sz
            in
                ( { model | screenSize = screenSize }, Cmd.none )


getPage : String -> ( Page, Msg )
getPage hash =
    let
        locs =
            String.split "/" hash

        emptyFunc =
            (\_ -> Cmd.none)
    in
        case locs of
            "#home" :: _ ->
                ( Home, RefreshActivities )

            "#blog" :: _ ->
                ( Blog, RefreshActivities )

            "#formulier" :: _ ->
                ( Form, None )

            "#inzendingen" :: uuid :: _ ->
                if (Uuid.isValidUuid uuid) then
                    ( Bets uuid, BetSelected )
                else
                    ( Ranking, None )

            "#inzendingen" :: _ ->
                ( Ranking, None )

            "#stand" :: _ ->
                ( Ranking, None )

            _ ->
                ( Home, RefreshActivities )


view : Model -> Html Msg
view model =
    let
        contentBase =
            case model.page of
                Home ->
                    viewHome model

                Blog ->
                    viewBlog model

                Ranking ->
                    viewRanking model

                Bets uuid ->
                    viewBet model uuid

                Form ->
                    viewForm model

        w =
            bodyWidth model.screenSize

        content =
            Element.column UI.Style.None
                [ padding (Size.margin model.screenSize) ]
                [ contentBase
                ]

        page =
            Element.column UI.Style.None
                []
                [ viewHeader model.screenSize
                , nav model.page model.screenSize
                , content
                ]
    in
        Element.el UI.Style.Page [] page
            |> Element.viewport UI.Style.stylesheet


viewHome : Model -> Element.Element UI.Style.Style variation Msg
viewHome model =
    Element.column UI.Style.None
        [ width (percent 100) ]
        [ Activities.viewCommentInput model
        , Activities.viewActivities model.activities
        ]


viewBlog : Model -> Element.Element UI.Style.Style variation Msg
viewBlog model =
    Element.column UI.Style.None
        []
        [ Activities.viewPostInput model
        , Activities.viewActivities model.activities
        ]


viewRanking : Model -> Element.Element UI.Style.Style variation Msg
viewRanking model =
    Element.text "Nog niet klaar"


viewBet : Model -> String -> Element.Element UI.Style.Style variation Msg
viewBet model uuid =
    case model.bet of
        NotAsked ->
            Element.text "Aan het ophalen."

        Loading ->
            Element.text "Aan het ophalen..."

        Failure err ->
            UI.Text.error "Oeps. Daar ging iets niet goed."

        Success bet ->
            Bets.View.viewBet bet model.screenSize


viewForm : Model -> Element.Element UI.Style.Style variation Msg
viewForm model =
    Element.paragraph UI.Style.None
        []
        [ Element.link "/voetbalpool/formulier" <| Element.el UI.Style.Link [] (Element.text "Klik hier om naar het formulier te gaan.")
        ]


nav2 : Element.Element UI.Style.Style variation Msg
nav2 =
    Element.navigation UI.Style.Menu
        [ paddingLeft 90, paddingBottom 20, spacing 30 ]
        { name = "Main Navigation"
        , options =
            [ Element.link "/voetbalpool/" (Element.el (UI.Style.NavLink UI.Style.Selected) [] (Element.text "home"))
            , Element.link "/voetbalpool/formulier/" (Element.el (UI.Style.NavLink UI.Style.Potential) [] (Element.text "formulier"))
            , Element.link "/voetbalpool/stand/" (Element.el (UI.Style.NavLink UI.Style.Potential) [] (Element.text "stand"))
            ]
        }


nav : Page -> ScreenSize -> Element.Element UI.Style.Style variation Msg
nav page screenSize =
    let
        comparePage newPage =
            case ( page, newPage ) of
                ( Home, Home ) ->
                    UI.Style.Selected

                ( Bets _, Bets _ ) ->
                    UI.Style.Selected

                ( Ranking, Ranking ) ->
                    UI.Style.Selected

                ( Form, Form ) ->
                    UI.Style.Selected

                _ ->
                    UI.Style.Potential

        pageLink newPage hash linkText =
            UI.Button.navLink (comparePage newPage) (Click hash) linkText
    in
        Element.navigation UI.Style.Menu
            [ paddingLeft (Size.margin screenSize), paddingBottom 20, spacing 30 ]
            { name = "Main Navigation"
            , options =
                [ pageLink Home "/voetbalpool/#home" "home"
                , pageLink Ranking "/voetbalpool/#stand" "stand"
                , pageLink Form "/voetbalpool/#formulier" "formulier"
                ]
            }


viewHeader : ScreenSize -> Element.Element UI.Style.Style variation Msg
viewHeader screenSize =
    let
        margin =
            Size.margin screenSize
    in
        Element.header
            (UI.Style.Title screenSize)
            [ width (percent 100), paddingXY (margin) (margin) ]
            (Element.text "De Voetbalpool")



-- app stuff


init : Flags -> Navigation.Location -> ( Model, Cmd Msg )
init flags loc =
    let
        ( page, msg ) =
            getPage loc.hash

        screenSize =
            classifyDevice { width = flags.width, height = 0 }

        model =
            { newModel | page = page, screenSize = screenSize }

        -- cmd =
        --     fetchActivities model
    in
        update msg model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ Window.resizes SetScreenSize ]


type alias Flags =
    { width : Int
    }


main : Program Flags Model Msg
main =
    Navigation.programWithFlags UrlChange
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }
