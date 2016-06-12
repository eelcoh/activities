module Main (..) where

import Effects exposing (Effects, Never)
import StartApp

import Markdown
import Html exposing (Html, div, button, text, textarea, input, section)
import Html.Events exposing (onClick, on, targetValue)
import Html.Attributes exposing (id, value, placeholder, class, href)

import Task exposing (Task)
import Http
import Date exposing (Month(..), Day(..))

import Json.Encode
import Json.Decode exposing (Decoder, (:=), object2, object4, andThen, maybe)


uriString : String
uriString =
  "/app/activities"

type alias Name = String
type alias Author = String
type alias Title = String
type alias Message = String
type alias UUID = String

type alias ActivityMeta =
  { date : Date.Date
  , active: Bool
  , uuid: String
  }

type Activity
  = ANewBet ActivityMeta Name UUID
  | AComment ActivityMeta Author Message
  | ABlog ActivityMeta Author Title Message
  | ANewRanking ActivityMeta


type Action
  = FetchActivities String
  | ReceiveActivities (List Activity)
  | FetchError Http.Error
  | SetCommentMsg String
  | SetCommentAuthor String
  | SaveComment
  | CommentSaved (Result Http.Error (List Activity))
  | HideCommentInput
  | ShowCommentInput


type alias Model =
  { activities : List Activity
  , comment : Comment
  , contents : Html
  , showComment: Bool
  }

type alias Comment =
  { author: String
  , msg : String
  }

newComment : Comment
newComment =
  {author = "", msg = ""}

newModel : Model
newModel =
  { activities = []
  , comment = newComment
  , contents = (div [] [text "niks"])
  , showComment = False
  }

fetchTask : Model -> String -> (Model, Effects Action)
fetchTask model urlString =
  let
    request = Task.map
      (\resp -> ReceiveActivities resp)
      (Http.get decode urlString)

    neverFailingRequest = Task.onError
      request
      (\err -> Task.succeed (FetchError err))
  in
    ( model, Effects.task neverFailingRequest )


saveComment : Model -> (Result Http.Error (List Activity) -> b) -> Effects.Effects b
saveComment model action =
  let

    (vrb, url) =
      ("POST", "/app/comments")

    body =
      encodeComment model.comment
      |> Json.Encode.encode 2
      |> Http.string

    response =
      Http.send Http.defaultSettings
        { verb = vrb
        , url = url
        , body = body
        , headers =
          [ ("Content-Type", "application/json")
          ]
        }

    decoded =
      Http.fromJson decode response
  in
    decoded
      |> Task.toResult
      |> Task.map action
      |> Effects.task


update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    FetchActivities urlString ->
      fetchTask model urlString

    ReceiveActivities activities ->
      ({ model | activities = activities}, Effects.none )

    FetchError err ->
      let
        contents =
          div [] [text (toString (Debug.log "err" err))]
      in
        ({ model | contents = contents}, Effects.none )

    SetCommentAuthor nwAuthor ->
      let
        oldComment =
          model.comment

        nwComment =
          {oldComment | author = nwAuthor}

      in
        ( {model | comment = nwComment }, Effects.none )

    ShowCommentInput ->
      ({model | showComment = True }, Effects.none)

    HideCommentInput ->
      ({model | showComment = False }, Effects.none)

    SetCommentMsg nwMsg ->
      let
        oldComment =
          model.comment

        nwComment =
          {oldComment | msg = nwMsg}

      in
        ( {model | comment = nwComment }, Effects.none )

    SaveComment ->
      let
        fx =
          saveComment model CommentSaved
      in
        (model, fx)

    CommentSaved rActivities ->
      case (Debug.log "rActivities" rActivities) of
        Ok activities ->
          let

            newModel =
              { model
              | activities = activities
              , comment = newComment
              , showComment = False
              }

          in
            (newModel, Effects.none)

        Err res ->
            (model, Effects.none)


view : Signal.Address Action -> Model -> Html
view address model =
  div []
    [ viewHeader
    , viewCommentInput address model
    , section [] (List.map (viewActivity address) model.activities)
    ]

viewHeader : Html
viewHeader =
  let
    paragraph =
      """Oranje was niet sterk genoeg om zich te plaatsen voor dit EK, maar
         dat weerhield ons er niet van toch een Voetbalpool te beginnen.
      """

    formlink =
      Html.p []
        [ text "Bekijk de "
        , Html.a [ href "/voetbalpool/stand", class "button-like right"] [ text "stand"]
        , text "."
        ]

  in
    section []
      [ Html.h1 [] [text "De Voetbalpool"]
      , Html.p [] [text paragraph]
      , formlink
      ]

viewCommentInput : Signal.Address Action -> Model -> Html
viewCommentInput address model =
  let
    commentInput v =
      div [] [
        textarea
          [ value v
          , placeholder "Het prikbord is open, ga je gang!"
          , on
              "input"
              targetValue
              (\val -> Signal.message address (SetCommentMsg val))
          ] []
      ]
    authorInput v =
      div [] [
        input
          [ value v
          , placeholder "naam"
          , on
              "input"
              targetValue
              (\val -> Signal.message address (SetCommentAuthor val))
          ] []
      ]

    saveButton =
      if ((model.comment.msg == "") || (model.comment.author == ""))
        then
          Html.p [class "xxxs"] [text "(Je moet beide velden invullen.)"]
        else
          Html.span [onClick address SaveComment, class "button right clickable xxxs"] [text "Prik!"]
  in
    if model.showComment
      then
        section []
          [ div [class "activity commentInput "]
            [ commentInput model.comment.msg
            , authorInput model.comment.author
            , div []
              [ saveButton
              , Html.p [class "xxxs"]
                [ Html.a [onClick address HideCommentInput, class "button-like right clickable"] [ text "Verberg"]
                , text " het prikbord."
                ]
              ]
            ]
          ]
      else
        section []
          [ Html.p []
            [ text "Zet vooral ook iets op het  "
            , Html.a [onClick address ShowCommentInput, class "button-like right clickable"] [ text "prikbord"]
            , text "."
            ]
          ]




viewActivity : Signal.Address Action -> Activity -> Html
viewActivity address activity =
  case activity of
    ANewBet activityMeta name uuid ->
      div
        [ class "activity new-bet"]
        [ div [] [text (name ++ " doet mee.")]
        , timeView activityMeta.date
        ]

    AComment activityMeta author msg ->
      div
        [ class "activity comment"]
        [ div [class "author"] [text (author ++ " zegt:")]
        , Markdown.toHtml msg
        , timeView activityMeta.date
        ]

    ABlog activityMeta author blogTitle msg ->
      div
        [class "activity blog"]
        [ div [class "blog-title"] [text blogTitle]
        , Markdown.toHtml msg
        , div [class "author"] [text author]
        , timeView activityMeta.date
        ]

    ANewRanking activityMeta ->
      div
        [ class "activity ranking"]
        [ div [] [text "De stand is bijgewerkt."]
        , timeView activityMeta.date
        ]




timeView : Date.Date -> Html
timeView dt =
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
      if n < 10
        then
          "0" ++ toString n
        else
          toString n

    dateString =
      dd ++ " " ++ (toString d) ++ " " ++ m ++ ", " ++ (toString h) ++ ":" ++ (twoDigitString mn)
  in
    div [class "date"] [text dateString]


-- app stuff

app : StartApp.App Model
app =
  StartApp.start
    { init = fetchTask newModel uriString
    , update = update
    , view = view
    , inputs = []
    }


main: Signal Html.Html
main =
  app.html


port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks


-- Json
encodeComment : Comment -> Json.Encode.Value
encodeComment comment =
  let
    encodedComment =
      Json.Encode.object
          [ ("author", Json.Encode.string comment.author)
          , ("msg", Json.Encode.string comment.msg)
          ]
  in
    Json.Encode.object
      [("comment", encodedComment)]

encodeActivity : Activity -> Json.Encode.Value
encodeActivity activity =
  case activity of
    AComment am author msg ->
      Json.Encode.object
        [ ("type", Json.Encode.string "comment")
        , ("author", Json.Encode.string author)
        , ("msg", Json.Encode.string msg)
        , ("meta", encodeActivityMeta am)
        ]

    ABlog am author title msg ->
      Json.Encode.object
        [ ("type", Json.Encode.string "blog")
        , ("author", Json.Encode.string author)
        , ("title", Json.Encode.string title)
        , ("msg", Json.Encode.string msg)
        , ("meta", encodeActivityMeta am)
        ]

    ANewRanking am->
      Json.Encode.object
        [ ("type", Json.Encode.string "new-ranking")
        , ("meta", encodeActivityMeta am)
        ]

    ANewBet am name betUuid ->
      Json.Encode.object
        [ ("type", Json.Encode.string "comment")
        , ("name", Json.Encode.string name)
        , ("bet-uuid", Json.Encode.string betUuid)
        , ("meta", encodeActivityMeta am)
        ]


encodeActivityMeta: ActivityMeta -> Json.Encode.Value
encodeActivityMeta am =
  Json.Encode.object
    [ ("date", Json.Encode.float (Date.toTime am.date))
    , ("active", Json.Encode.bool am.active)
    , ("uuid", Json.Encode.string am.uuid)
    ]

type alias IncomingActivities = { activities: List Activity }

{-
decode : Decoder (List Activity)
decode =
  Json.Decode.map
    (\x -> x.activities)
    decodeIncoming
  -}

decode : Decoder (List Activity)
decode =
  ("activities" := Json.Decode.list decodeActivity)

decodeActivity : Decoder Activity
decodeActivity =
  ("type" := Json.Decode.string) `andThen` decodeActivityDetails

decodeActivityDetails : String -> Decoder Activity
decodeActivityDetails tp =
  case tp of
    "comment" ->
      Json.Decode.object3 AComment
        ("meta" := decodeMeta)
        ("author" := Json.Decode.string)
        ("msg" := Json.Decode.string)

    "blog" ->
      Json.Decode.object4 ABlog
        ("meta" := decodeMeta)
        ("author" := Json.Decode.string)
        ("title" := Json.Decode.string)
        ("msg" := Json.Decode.string)

    "new-bet" ->
      Json.Decode.object3 ANewBet
        ("meta" := decodeMeta)
        ("name" := Json.Decode.string)
        ("bet-uuid" := Json.Decode.string)

    "new-ranking" ->
      Json.Decode.object1 ANewRanking
        ("meta" := decodeMeta)

    _ ->
      Json.Decode.fail "WHOOPS"

decodeMeta: Decoder ActivityMeta
decodeMeta =
  Json.Decode.object3 ActivityMeta
    ("date" := decodeDate)
    ("active" := Json.Decode.bool)
    ("uuid" := Json.Decode.string)

decodeDate : Decoder Date.Date
decodeDate =
  Json.Decode.float
  |> Json.Decode.map Date.fromTime
