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
import Authentication
import Ranking
import Results
import Knockouts
import DataStatus
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
    , credentials = Empty
    , token = NotAsked
    , ranking = NotAsked
    , rankingDetails = NotAsked
    , matchResults = NotAsked
    , matchResult = NotAsked
    , knockoutsResults = Fresh NotAsked
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
                    case model.token of
                        RemoteData.Success (Token token) ->
                            Activities.savePost model token

                        _ ->
                            Cmd.none
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

        ViewRankingDetails uuid ->
            let
                url =
                    "#stand/" ++ uuid

                cmd =
                    Navigation.newUrl url
            in
                ( model, cmd )

        RetrieveRankingDetails uuid ->
            let
                cmd =
                    Ranking.fetchRankingDetails uuid
            in
                ( model, cmd )

        FetchedRankingDetails results ->
            ( { model | rankingDetails = results }, Cmd.none )

        RefreshActivities ->
            ( model, Activities.fetchActivities model )

        RefreshRanking ->
            case model.ranking of
                Success _ ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Ranking.fetchRanking )

        SetScreenSize sz ->
            let
                screenSize =
                    classifyDevice sz
            in
                ( { model | screenSize = screenSize }, Cmd.none )

        SetUsername uid ->
            let
                newCredentials =
                    case model.credentials of
                        Empty ->
                            WithUsername uid

                        WithPassword pw ->
                            Submittable uid pw

                        WithUsername uid ->
                            WithUsername uid

                        Submittable _ pw ->
                            Submittable uid pw
            in
                ( { model | credentials = newCredentials }, Cmd.none )

        SetPassword pw ->
            let
                newCredentials =
                    case model.credentials of
                        Empty ->
                            WithPassword pw

                        WithPassword _ ->
                            WithPassword pw

                        WithUsername uid ->
                            Submittable uid pw

                        Submittable uid _ ->
                            Submittable uid pw
            in
                ( { model | credentials = newCredentials }, Cmd.none )

        FetchedToken token ->
            let
                newCredentials =
                    case token of
                        Success _ ->
                            Empty

                        _ ->
                            model.credentials
            in
                ( { model | token = token, credentials = newCredentials }, Cmd.none )

        Authenticate ->
            case model.credentials of
                Submittable uid pw ->
                    ( model, Authentication.authenticate uid pw )

                _ ->
                    ( model, Cmd.none )

        RecreateRanking ->
            let
                cmd =
                    case model.token of
                        RemoteData.Success token ->
                            Ranking.recreate token

                        _ ->
                            Cmd.none
            in
                ( model, cmd )

        FetchedRanking ranking ->
            ( { model | ranking = ranking }, Cmd.none )

        FetchedMatchResults results ->
            let
                nwModel =
                    case results of
                        Failure e ->
                            let
                                d =
                                    Debug.log (Basics.toString e)
                            in
                                { model | matchResults = results }

                        _ ->
                            { model | matchResults = results }
            in
                ( nwModel, Cmd.none )

        StoredMatchResult result ->
            ( { model | matchResult = result }, Results.fetchMatchResults )

        RefreshResults ->
            case model.matchResults of
                Success _ ->
                    ( model, Cmd.none )

                _ ->
                    ( model, Results.fetchMatchResults )

        EditMatch match ->
            let
                url =
                    "#wedstrijden/wedstrijd/" ++ match.match

                cmd =
                    Navigation.newUrl url
            in
                ( { model | matchResult = Success match, page = EditMatchResult }, cmd )

        UpdateMatchResult match ->
            case model.token of
                Success token ->
                    let
                        cmd =
                            Results.updateMatchResults token match
                    in
                        ( model, cmd )

                _ ->
                    ( model, Cmd.none )

        CancelMatchResult match ->
            case model.token of
                Success token ->
                    let
                        canceledMatch =
                            { match | score = Nothing }

                        cmd =
                            Results.updateMatchResults token canceledMatch
                    in
                        ( model, cmd )

                _ ->
                    ( model, Cmd.none )

        FetchedKnockoutsResults results ->
            ( { model | knockoutsResults = Fresh results }, Cmd.none )

        StoredKnockoutsResults results ->
            ( { model | knockoutsResults = Fresh results }, Cmd.none )

        Qualify rnd q team ->
            let
                kos =
                    DataStatus.map (Knockouts.update rnd q team) model.knockoutsResults
            in
                ( { model | knockoutsResults = kos }, Cmd.none )

        UpdateKnockoutsResults ->
            let
                cmd =
                    case ( model.knockoutsResults, model.token ) of
                        ( Dirty (Success res), Success token ) ->
                            Knockouts.updateKnockoutsResults token res

                        _ ->
                            Cmd.none
            in
                ( model, cmd )

        InitialiseKnockoutsResults ->
            let
                cmd =
                    case (model.token) of
                        Success token ->
                            Knockouts.inititaliseKnockoutsResults token

                        _ ->
                            Cmd.none
            in
                ( model, cmd )

        RefreshKnockoutsResults ->
            let
                cmd =
                    Knockouts.fetchKnockoutsResults
            in
                ( model, cmd )


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

            "#stand" :: uuid :: _ ->
                if (Uuid.isValidUuid uuid) then
                    ( RankingDetailsView, RetrieveRankingDetails uuid )
                else
                    ( Ranking, None )

            "#stand" :: _ ->
                ( Ranking, RefreshRanking )

            "#wedstrijden" :: "wedstrijd" :: _ ->
                ( EditMatchResult, None )

            "#wedstrijden" :: [] ->
                ( Results, RefreshResults )

            "#knockouts" :: [] ->
                ( KOResults, RefreshKnockoutsResults )

            "#login" :: _ ->
                ( Login, None )

            _ ->
                let
                    page =
                        Debug.log "page" locs
                in
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
                    Ranking.viewRanking model

                RankingDetailsView ->
                    Ranking.viewRankingDetails model

                Results ->
                    Results.view model

                EditMatchResult ->
                    Results.edit model

                KOResults ->
                    Knockouts.view model

                Bets uuid ->
                    viewBet model uuid

                Form ->
                    viewForm model

                Login ->
                    Authentication.viewLoginForm model

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
                , viewNav model.token model.page model.screenSize
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


viewNav : WebData Token -> Page -> ScreenSize -> Element.Element UI.Style.Style variation Msg
viewNav token page screenSize =
    let
        options =
            case token of
                Success _ ->
                    authenticatedOptions page screenSize

                _ ->
                    unauthenticatedOptions page screenSize
    in
        Element.navigation UI.Style.Menu
            [ paddingLeft (Size.margin screenSize), paddingBottom 20, spacing 30 ]
            { name = "Main Navigation"
            , options = options
            }


comparePage : Page -> Page -> UI.Style.ButtonSemantics
comparePage page newPage =
    case ( page, newPage ) of
        ( Home, Home ) ->
            UI.Style.Selected

        ( Bets _, Bets _ ) ->
            UI.Style.Selected

        ( Ranking, Ranking ) ->
            UI.Style.Selected

        ( Results, Results ) ->
            UI.Style.Selected

        ( Form, Form ) ->
            UI.Style.Selected

        ( Blog, Blog ) ->
            UI.Style.Selected

        ( Login, Login ) ->
            UI.Style.Selected

        _ ->
            UI.Style.Potential


unauthenticatedOptions : Page -> ScreenSize -> List (Element.Element UI.Style.Style variation Msg)
unauthenticatedOptions page screenSize =
    let
        pageLink newPage hash linkText =
            UI.Button.navLink (comparePage page newPage) (Click hash) linkText
    in
        [ pageLink Home "/voetbalpool/#home" "home"
        , pageLink Ranking "/voetbalpool/#stand" "stand"
        , pageLink Results "/voetbalpool/#wedstrijden" "wedstrijden"
          --, pageLink Form "/voetbalpool/#formulier" "formulier"
        ]


authenticatedOptions : Page -> ScreenSize -> List (Element.Element UI.Style.Style variation Msg)
authenticatedOptions page screenSize =
    let
        pageLink newPage hash linkText =
            UI.Button.navLink (comparePage page newPage) (Click hash) linkText
    in
        [ pageLink Home "/voetbalpool/#home" "home"
        , pageLink Ranking "/voetbalpool/#stand" "stand"
        , pageLink Results "/voetbalpool/#wedstrijden" "wedstrijden"
        , pageLink KOResults "/voetbalpool/#knockouts" "knockouts"
          -- , pageLink Form "/voetbalpool/#formulier" "formulier"
        , pageLink Blog "/voetbalpool/#blog" "blog"
        ]


nav : List (Element.Element UI.Style.Style variation msg) -> ScreenSize -> Element.Element UI.Style.Style variation msg
nav options screenSize =
    Element.navigation UI.Style.Menu
        [ paddingLeft (Size.margin screenSize), paddingBottom 20, spacing 30 ]
        { name = "Main Navigation"
        , options = options
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
