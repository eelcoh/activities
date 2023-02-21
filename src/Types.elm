module Types exposing (..)

import Bets.Types exposing (Bet)
import Browser.Navigation as Navigation
import Date exposing (Day(..), Month(..))
import Html exposing (Html)
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
    | RankingDetailsView
    | Bets String
    | Login
    | Results
    | EditMatchResult
    | KOResults
    | TSResults


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
    | RecreateRanking
    | FetchedRanking (WebData RankingSummary)
    | RefreshRanking
    | FetchedRankingDetails (WebData RankingDetails)
    | ViewRankingDetails String
    | RetrieveRankingDetails String
    | FetchedMatchResults (WebData MatchResults)
    | RefreshResults
    | EditMatch MatchResult
    | UpdateMatchResult MatchResult
    | CancelMatchResult MatchResult
    | StoredMatchResult (WebData MatchResult)
    | FetchedKnockoutsResults (WebData KnockoutsResults)
    | StoredKnockoutsResults (WebData KnockoutsResults)
    | Qualify Bets.Types.Round Bets.Types.HasQualified Bets.Types.Team
    | UpdateKnockoutsResults
    | InitialiseKnockoutsResults
    | RefreshKnockoutsResults
    | ChangeQualify Bets.Types.Round Bets.Types.HasQualified Bets.Types.Team
    | RefreshTopscorerResults
    | ChangeTopscorerResults Bets.Types.HasQualified Topscorer
    | UpdateTopscorerResults
    | InitialiseTopscorerResults
    | FetchedTopscorerResults (WebData TopscorerResults)
    | StoredTopscorerResults (WebData TopscorerResults)


type Qualified
    = Did
    | DidNot
    | NotYet


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
    , ranking : WebData RankingSummary
    , rankingDetails : WebData RankingDetails
    , matchResults : WebData MatchResults
    , matchResult : WebData MatchResult
    , knockoutsResults : DataStatus (WebData KnockoutsResults)
    , topscorerResults : DataStatus (WebData TopscorerResults)
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


type alias RankingSummary =
    { summary : List RankingGroup
    , time : Date.Date
    }


type alias RankingGroup =
    { pos : Int
    , bets : List RankingSummaryLine
    , total : Int
    }


type alias RankingSummaryLine =
    { name : String
    , rounds : List RoundScore
    , topscorer : Int
    , total : Int
    , uuid : String
    }


type alias RankingDetails =
    { name : String
    , rounds : List RoundScore
    , topscorer : Int
    , total : Int
    , uuid : String
    , bet : Bets.Types.Bet
    }


type alias RoundScore =
    { round : String
    , points : Int
    }


type alias MatchResults =
    { results : List MatchResult }


type alias MatchResult =
    { matchResultId : String
    , match : String
    , homeTeam : Bets.Types.Team
    , awayTeam : Bets.Types.Team
    , score : Maybe Bets.Types.Score
    }


type Access
    = Unauthorised
    | Authorised



-- type KnockoutsResults struct {
-- 	Teams map[string]TeamRounds `json:"team"`
-- }
-- type TeamRounds struct {
-- 	Team            Team            `json:"team"`
-- 	RoundsQualified RoundsQualified `json:"roundsQualified"`
-- }
-- type RoundsQualified map[string]string


type alias KnockoutsResults =
    { teams : List ( String, TeamRounds )
    }


type alias TeamRounds =
    { team : Bets.Types.Team
    , roundsQualified : List ( Bets.Types.Round, Bets.Types.HasQualified )
    }


type alias TopscorerResults =
    { topscorers : List ( Bets.Types.HasQualified, Topscorer )
    }


type alias Topscorer =
    { team : Bets.Types.Team
    , topscorer : String
    }



-- type alias Knockouts =
--     { teamsIn : List Bets.Types.Team
--     , teamsOut : List Bets.Types.Team
--     }


type DataStatus a
    = Fresh a
    | Dirty a
    | Stale a
