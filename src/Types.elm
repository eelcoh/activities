module Types exposing (..)

import Html exposing (Html)
import Date exposing (Month(..), Day(..))
import Navigation
import Bets.Types exposing (Bet)
import RemoteData exposing (WebData)
import Window


type alias Name =
    String


type alias Author =
    String


type alias Title =
    String


type alias Message =
    String


type alias UUID =
    String


type Page
    = Home
    | Blog
    | Form
    | Ranking
    | Bets String
    | Login


type alias ActivityMeta =
    { date : Date.Date
    , active : Bool
    , uuid : String
    }


type Activity
    = ANewBet ActivityMeta Name UUID
    | AComment ActivityMeta Author Message
    | APost ActivityMeta Author Title Message
    | ANewRanking ActivityMeta


type Msg
    = FetchedActivities (WebData (List Activity))
    | FetchedBet (WebData Bet)
    | SetCommentMsg String
    | SetCommentAuthor String
    | SaveComment
    | SavedComment (WebData (List Activity))
    | HideCommentInput
    | ShowCommentInput
    | SetPostTitle String
    | SetPostPassphrase String
    | SetPostMsg String
    | SetPostAuthor String
    | SavePost
    | SavedPost (WebData (List Activity))
    | HidePostInput
    | ShowPostInput
    | None
    | UrlChange Navigation.Location
    | Click String
    | RefreshActivities
    | BetSelected
    | SetScreenSize Window.Size
    | SetUsername String
    | SetPassword String
    | Authenticate
    | FetchedToken (WebData Token)


type alias Model =
    { activities : WebData (List Activity)
    , comment : Comment
    , post : Post
    , contents : Html Msg
    , showComment : Bool
    , showPost : Bool
    , page : Page
    , bet : WebData Bets.Types.Bet
    , credentials : Credentials
    , token : WebData Token
    , screenSize : ScreenSize
    }


type alias Comment =
    { author : String
    , msg : String
    }


type alias Post =
    { author : String
    , title : String
    , msg : String
    , passphrase : String
    }


type ScreenSize
    = Small
    | Big


type alias Creds =
    { username : String
    , password : String
    }


type Credentials
    = Empty
    | WithUsername String
    | WithPassword String
    | Submittable String String


type Token
    = Token String
