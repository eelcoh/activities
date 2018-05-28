module Bets.Types
    exposing
        ( Bet
        , Answer
        , AnswerID
        , AnswerT(..)
        , Answers
        , Candidates
        , BestThird
        , BestThirds
        , Draw
        , DrawID
        , NextID
        , Participant
        , Topscorer
        , Score
        , Points
        , DateString
        , Team
        , TeamID
        , Match
        , Stadium
        , Pos(..)
        , Group(..)
        , Round(..)
        , TeamData
        , TeamDatum
        , Player
        , Bracket(..)
        , Slot
        , Winner(..)
        , Qualifier
        , HasQualified(..)
        )

import Date


type alias AnswerID =
    String


type alias Answer =
    ( AnswerID, AnswerT )


type alias Answers =
    List Answer


type alias Bet =
    { answers : Answers
    , betId : Maybe Int
    , uuid : Maybe String
    , active : Bool
    }


type alias Points =
    Maybe Int


type alias DateString =
    String


type alias TeamID =
    String


type alias Team =
    { teamID : TeamID
    , teamName : String
    }


type Group
    = A
    | B
    | C
    | D
    | E
    | F
    | G
    | H


type Round
    = I
    | II
    | III
    | IV
    | V
    | VI


type alias Topscorer =
    ( Maybe String, Maybe Team )


type alias Player =
    String


type alias TeamWithPlayers a =
    { a | players : List Player }


type alias TeamsWithPlayers a =
    List (TeamWithPlayers a)


type alias TeamDatum =
    { team : Team
    , players : List Player
    , group : Group
    }


type alias TeamData =
    List TeamDatum


type alias Score =
    ( Maybe Int, Maybe Int )


type alias DrawID =
    String


type alias NextID =
    Maybe DrawID


type alias Draw =
    ( DrawID, Maybe Team )


type alias BestThird =
    ( Group, Team, DrawID )


type alias BestThirds =
    List BestThird


type alias Stadium =
    { stadium : String
    , town : String
    }


type alias Match =
    ( Draw, Draw, Date.Date, Stadium )



{- Types for Bracket -}


type Bracket
    = MatchNode Slot Winner Home Away Round HasQualified
    | TeamNode Slot Qualifier HasQualified


type HasQualified
    = In
    | Out
    | TBD


type Winner
    = HomeTeam
    | AwayTeam
    | None


type alias Home =
    Bracket


type alias Away =
    Bracket


type alias Qualifier =
    Maybe Team


type alias Slot =
    DrawID



{- Types for GroupPosition -}


type Pos
    = First
    | Second
    | Third
    | TopThird
    | Free


type alias Candidates =
    List ( Group, Team, Pos )



{- Types for Answers and Bet -}


type AnswerT
    = AnswerGroupMatch Group Match (Maybe Score) Points
    | AnswerGroupPosition Group Pos Draw Points
    | AnswerGroupBestThirds BestThirds Points
    | AnswerMatchWinner Round Match NextID (Maybe Team) Points
    | AnswerBracket Bracket Points
    | AnswerTopscorer Topscorer Points
    | AnswerParticipant Participant



{- Types for participants -}


type alias Participant =
    { name : Maybe String
    , address : Maybe String
    , residence : Maybe String
    , phone : Maybe String
    , email : Maybe String
    , howyouknowus : Maybe String
    }



{-
   type alias QAndAId = String
   type QAndA QAndAId Question (Maybe Answer) Points

   type Question
     = QMatch Group Draw Draw (Result String Date.Date) Stadium (Maybe Score) Points
     | QGroupPosition Group Pos Draw Points
     | QGroupBestThirds (Maybe Group) Draw Points
     | QMatchWinner Round Draw Draw (Result String Date.Date) Stadium NextID (Maybe Team) Points
     | QTopscorer Topscorer Points
     | QParticipant (Maybe Participant)

   type A
     = AScore Score
     = ATeam Team
     = AParticipant (Maybe Participant)
     = ATopscorer Topscorer
-}
