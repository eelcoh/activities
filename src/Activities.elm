module Activities exposing (..)

import Types exposing (Model, Activity(..), Msg(..), Comment, ActivityMeta, Post)
import RemoteData exposing (RemoteData(..), WebData)
import RemoteData.Http as Web
import Http
import Json.Encode
import Json.Decode exposing (Decoder, andThen, maybe, field)
import Element exposing (column, row)
import Element.Input as Input
import Element.Events as Events
import Element.Attributes exposing (px, padding, paddingLeft, paddingTop, paddingBottom, paddingXY, spacing, alignLeft, verticalSpread, center, alignRight, width, height)
import UI.Style
import UI.Button
import UI.Text
import Markdown
import Date exposing (Month(..), Day(..))
import UI.Size exposing (bodyWidth)


fetchActivities : Model -> Cmd Msg
fetchActivities model =
    Web.get "/activities" FetchedActivities decode


saveComment : Model -> Cmd Msg
saveComment model =
    let
        comment =
            encodeComment model.comment
    in
        Web.post "/activities/comments" SavedComment decode comment


savePost : Model -> String -> Cmd Msg
savePost model token =
    let
        post =
            encodePost model.post

        bearer =
            "Bearer " ++ token

        header =
            Http.header "Authorization" bearer

        config =
            { headers = [ header ]
            , withCredentials = True
            , timeout = Nothing
            }
    in
        Web.postWithConfig config "/activities/blogs" SavedPost decode post


viewActivities : WebData (List Activity) -> Element.Element UI.Style.Style variation Msg
viewActivities wActivities =
    case wActivities of
        NotAsked ->
            Element.text "Aan het ophalen."

        Loading ->
            Element.text "Aan het ophalen..."

        Failure err ->
            UI.Text.error "Oeps. Daar ging iets niet goed."

        Success activities ->
            List.map viewActivity activities
                |> Element.column UI.Style.None [ spacing 20 ]


viewActivity : Activity -> Element.Element UI.Style.Style variation Msg
viewActivity activity =
    case activity of
        ANewBet activityMeta name uuid ->
            Element.el UI.Style.CommentBox [ padding 20 ] (Element.text (name ++ "doet mee"))

        AComment activityMeta author comment ->
            commentBox author comment activityMeta.date

        APost activityMeta author blogTitle blog ->
            blogBox author blogTitle blog activityMeta.date

        ANewRanking activityMeta ->
            Element.el UI.Style.CommentBox [ padding 20 ] (Element.text ("De stand is bijgewerkt"))


blogBox : String -> String -> String -> Date.Date -> Element.Element UI.Style.Style variation Msg
blogBox author title blog date =
    column UI.Style.PostBox
        [ verticalSpread, padding 20 ]
        [ Element.paragraph UI.Style.PostTitle [] [ (Element.text title) ]
        , blogView blog
        , Element.el UI.Style.AuthorText [ alignRight ] (Element.text (author ++ ", " ++ (dateText date)))
        ]


commentBox : String -> String -> Date.Date -> Element.Element UI.Style.Style variation Msg
commentBox author comment date =
    column UI.Style.CommentBox
        [ verticalSpread, padding 20 ]
        [ row UI.Style.None
            [ alignLeft ]
            [ Element.el UI.Style.AuthorText [] (Element.text (author ++ " zegt:")) ]
        , row UI.Style.None
            [ alignLeft ]
            [ commentView comment ]
        , row UI.Style.None
            [ alignRight ]
            [ timeView date ]
        ]


blogView : String -> Element.Element UI.Style.Style variation Msg
blogView c =
    let
        comment =
            Markdown.toHtml [] c
                |> Element.html
    in
        Element.el UI.Style.PostText [] comment


commentView : String -> Element.Element UI.Style.Style variation Msg
commentView c =
    let
        comment =
            Markdown.toHtml [] c
                |> Element.html
    in
        Element.el UI.Style.CommentText [] comment


timeView : Date.Date -> Element.Element UI.Style.Style variation Msg
timeView dt =
    Element.el UI.Style.DateText [] (Element.text (dateText dt))


dateText : Date.Date -> String
dateText dt =
    let
        m =
            Date.month dt
                |> toMonth

        d =
            Date.day dt

        dd =
            Date.dayOfWeek dt
                |> toDay

        h =
            Date.hour dt

        mn =
            Date.minute dt

        toMonth mon =
            case mon of
                Jan ->
                    "januari"

                Feb ->
                    "februari"

                Mar ->
                    "maart"

                Apr ->
                    "april"

                May ->
                    "mei"

                Jun ->
                    "juni"

                Jul ->
                    "juli"

                Aug ->
                    "augustus"

                Sep ->
                    "september"

                Oct ->
                    "oktober"

                Nov ->
                    "november"

                Dec ->
                    "december"

        toDay day =
            case day of
                Mon ->
                    "maandag"

                Tue ->
                    "dinsdag"

                Wed ->
                    "woensdag"

                Thu ->
                    "donderdag"

                Fri ->
                    "vrijdag"

                Sat ->
                    "zaterdag"

                Sun ->
                    "zondag"

        twoDigitString n =
            if n < 10 then
                "0" ++ toString n
            else
                toString n

        dateString =
            dd ++ " " ++ (toString d) ++ " " ++ m ++ ", " ++ (toString h) ++ ":" ++ (twoDigitString mn)
    in
        dateString


viewCommentInput : Model -> Element.Element UI.Style.Style variation Msg
viewCommentInput model =
    let
        placeholder p =
            Input.placeholder { text = p, label = Input.hiddenLabel p }

        commentInput v =
            let
                area =
                    { onChange = (\val -> SetCommentMsg val)
                    , value = v
                    , label = placeholder "Tekst"
                    , options = []
                    }
            in
                Input.multiline UI.Style.TextInput [ height (px 120) ] area

        commentInputTrap =
            Element.paragraph UI.Style.None
                [ paddingXY 0 0, spacing 0 ]
                [ Element.text "Schrijf vooral iets op "
                , UI.Button.pill UI.Style.Trap ShowCommentInput "het prikbord"
                ]

        commentInputTrap2 v =
            let
                area =
                    { onChange = (\val -> SetCommentMsg val)
                    , value = ""
                    , label = placeholder v
                    , options = []
                    }
            in
                Input.multiline UI.Style.TextInput [ Events.onFocus ShowCommentInput, height (px 36) ] area

        authorInput v =
            let
                area =
                    { onChange = (\val -> SetCommentAuthor val)
                    , value = v
                    , label = placeholder "Naam"
                    , options = []
                    }
            in
                Input.text UI.Style.TextInput [ height (px 36) ] area

        saveButton =
            if ((model.comment.msg == "") || (model.comment.author == "")) then
                UI.Button.pill UI.Style.Inactive None "je moet beide velden invullen"
            else
                UI.Button.pill UI.Style.Active SaveComment "prik!"

        input =
            if model.showComment then
                Element.column UI.Style.CommentInputBox
                    [ padding 10, spacing 20 ]
                    [ commentInput model.comment.msg
                    , authorInput model.comment.author
                    , saveButton
                    ]
            else
                commentInputTrap

        -- Element.column UI.Style.CommentInputBox
        --     [ padding 10, spacing 20 ]
        --     [ commentInputTrap
        --     ]
    in
        Element.el UI.Style.None [ paddingXY 0 0 ] input


viewPostInput : Model -> Element.Element UI.Style.Style variation Msg
viewPostInput model =
    let
        placeholder p =
            Input.placeholder { text = p, label = Input.hiddenLabel p }

        titleInput v =
            let
                area =
                    { onChange = (\val -> SetPostTitle val)
                    , value = v
                    , label = placeholder "Titel"
                    , options = []
                    }
            in
                Input.text UI.Style.TextInput [ height (px 36) ] area

        postInput v =
            let
                area =
                    { onChange = (\val -> SetPostMsg val)
                    , value = v
                    , label = placeholder "Tekst"
                    , options = []
                    }
            in
                Input.multiline UI.Style.TextInput [ height (px 200) ] area

        postInputTrap =
            let
                area =
                    { onChange = (\_ -> None)
                    , value = ""
                    , label = placeholder "Nieuwe Blog post."
                    , options = []
                    }
            in
                Input.text UI.Style.TrapInput [ Events.onFocus ShowPostInput, height (px 36) ] area

        passphraseInput v =
            let
                area =
                    { onChange = (\val -> SetPostPassphrase val)
                    , value = v
                    , label = placeholder "Wachtwoord"
                    , options = []
                    }
            in
                Input.text UI.Style.TextInput [ height (px 36) ] area

        authorInput v =
            let
                area =
                    { onChange = (\val -> SetPostAuthor val)
                    , value = v
                    , label = placeholder "Naam"
                    , options = []
                    }
            in
                Input.text UI.Style.TextInput [ height (px 36) ] area

        saveButton =
            if ((model.post.msg == "") || (model.post.author == "") || (model.post.passphrase == "")) then
                UI.Button.pill UI.Style.Inactive None "je moet alle velden invullen"
            else
                UI.Button.pill UI.Style.Active SavePost "post!"

        input =
            if model.showPost then
                Element.column UI.Style.PostInputBox
                    [ padding 20, spacing 20 ]
                    [ titleInput model.post.title
                    , postInput model.post.msg
                    , passphraseInput model.post.passphrase
                    , authorInput model.post.author
                    , saveButton
                    ]
            else
                Element.column UI.Style.PostInputBox
                    [ padding 20, spacing 20 ]
                    [ postInputTrap
                    ]
    in
        Element.el UI.Style.None [ paddingXY 20 20 ] input



-- Json


encodeComment : Comment -> Json.Encode.Value
encodeComment comment =
    let
        encodedComment =
            Json.Encode.object
                [ ( "author", Json.Encode.string comment.author )
                , ( "msg", encodeMessage comment.msg )
                ]

        multlineMsg =
            String.split "\n" comment.msg
    in
        Json.Encode.object
            [ ( "comment", encodedComment ) ]


encodePost : Post -> Json.Encode.Value
encodePost post =
    let
        encodedPost =
            Json.Encode.object
                [ ( "author", Json.Encode.string post.author )
                , ( "title", Json.Encode.string post.title )
                , ( "msg", encodeMessage post.msg )
                , ( "passphrase", Json.Encode.string post.passphrase )
                ]
    in
        Json.Encode.object
            [ ( "blog", encodedPost ) ]


encodeMessage : String -> Json.Encode.Value
encodeMessage msg =
    let
        multlineMsg =
            String.split "\n" msg
    in
        Json.Encode.list (List.map Json.Encode.string multlineMsg)


encodeActivity : Activity -> Json.Encode.Value
encodeActivity activity =
    case activity of
        AComment am author msg ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "comment" )
                , ( "author", Json.Encode.string author )
                , ( "msg", encodeMessage msg )
                , ( "meta", encodeActivityMeta am )
                ]

        APost am author title msg ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "blog" )
                , ( "author", Json.Encode.string author )
                , ( "title", Json.Encode.string title )
                , ( "msg", encodeMessage msg )
                , ( "meta", encodeActivityMeta am )
                ]

        ANewRanking am ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "new-ranking" )
                , ( "meta", encodeActivityMeta am )
                ]

        ANewBet am name betUuid ->
            Json.Encode.object
                [ ( "type", Json.Encode.string "comment" )
                , ( "name", Json.Encode.string name )
                , ( "bet-uuid", Json.Encode.string betUuid )
                , ( "meta", encodeActivityMeta am )
                ]


encodeActivityMeta : ActivityMeta -> Json.Encode.Value
encodeActivityMeta am =
    Json.Encode.object
        [ ( "date", Json.Encode.float (Date.toTime am.date) )
        , ( "active", Json.Encode.bool am.active )
        , ( "uuid", Json.Encode.string am.uuid )
        ]


type alias IncomingActivities =
    { activities : List Activity }


decode : Decoder (List Activity)
decode =
    (field "activities" (Json.Decode.list decodeActivity))


decodeActivity : Decoder Activity
decodeActivity =
    (field "type" Json.Decode.string) |> andThen decodeActivityDetails


decodeActivityDetails : String -> Decoder Activity
decodeActivityDetails tp =
    case tp of
        "comment" ->
            Json.Decode.map3 AComment
                (field "meta" decodeMeta)
                (field "author" Json.Decode.string)
                (field "msg" decodeMessage)

        "blog" ->
            Json.Decode.map4 APost
                (field "meta" decodeMeta)
                (field "author" Json.Decode.string)
                (field "title" Json.Decode.string)
                (field "msg" decodeMessage)

        "new-bet" ->
            Json.Decode.map3 ANewBet
                (field "meta" decodeMeta)
                (field "name" Json.Decode.string)
                (field "bet-uuid" Json.Decode.string)

        "new-ranking" ->
            Json.Decode.map ANewRanking
                (field "meta" decodeMeta)

        _ ->
            Json.Decode.fail "WHOOPS"


decodeMeta : Decoder ActivityMeta
decodeMeta =
    Json.Decode.map3 ActivityMeta
        (field "date" decodeDate)
        (field "active" Json.Decode.bool)
        (field "uuid" Json.Decode.string)


decodeDate : Decoder Date.Date
decodeDate =
    Json.Decode.float
        |> Json.Decode.map Date.fromTime


decodeMessage : Decoder String
decodeMessage =
    Json.Decode.list Json.Decode.string
        |> Json.Decode.map (String.join "\n")
