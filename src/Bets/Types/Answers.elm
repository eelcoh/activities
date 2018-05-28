module Bets.Types.Answers  exposing
  ( getAnswer
  , setTeam
  , setMatchScore
  , setTopscorer
  , setParticipant
  , setMatchWinner
  , setWinner
  , findGroupMatchAnswers
  , findGroupPositionAnswers
  , findAllGroupMatchAnswers
  , encode
  , decode
  )

import Bets.Types exposing (..)
import Bets.Types.Round exposing (nextRound, isSameOrANextRound)
import Bets.Types.Match exposing (..)
import Bets.Types.Bracket as B
import Bets.Types.Score as S
import Bets.Types.BestThirds
import Bets.Types.Answer

import Dict

import Json.Encode
import Json.Decode exposing (Decoder)


-- alias so we can write a => b to get (a, b)
(=>) : a -> b -> (a, b)
(=>) = (,)

-- EXPOSED
getAnswer : Answers -> AnswerID -> Maybe Answer
getAnswer answers answerID =
  List.filter (\(id, _) -> id == answerID) answers
  |> List.head

{-
  Set the score for a groupmatch question.
-}
setMatchScore : Answers -> Answer -> Score -> Answers
setMatchScore answers (answerID, answer) score =
  let
    mergeScores mCurScore newScore =
      case mCurScore of
        Nothing ->
          Just newScore
        Just curScore ->
          S.merge newScore curScore
          |> Just

    newAnswer =
      case answer of
        AnswerGroupMatch grp match s points ->
          let
            newScore =
              mergeScores s score
          in
            answerID => AnswerGroupMatch grp match newScore points

        _ ->
          answerID => answer
  in
    setAnswer newAnswer answers


setMatchWinner : Answers -> Answer -> Team -> Answers
setMatchWinner answers (answerID, answer) team =
  case answer of

    AnswerMatchWinner rnd match nextID mTeam points ->
      let
        teamInMatch =
          List.member team (teamsInMatch match)

        newTeam =
          case mTeam of
            Nothing ->
              Nothing
            Just t ->
              if t == team
                then
                  Nothing
                else
                  Just team

        newAnswers =
          case mTeam of
            Nothing ->
              setNext nextID team answers
            Just t ->
              if t /= team
                then
                  -- unsetTeam (nextRound rnd) team answers
                  unsetTeams answers (Just rnd) [t]
                  |> setNext nextID team
                else
                  unsetTeams answers (Just rnd) [team]

        newAnswer =
          if teamInMatch
            then
              AnswerMatchWinner rnd match nextID newTeam points
            else
              answer

      in
        if teamInMatch
          then
            setAnswer (answerID, newAnswer) newAnswers
          else
            answers

    _ -> -- catch all
      answers
{-
  @@@
-}
setTeam : Answers -> Answer -> Group -> Team -> Answers
setTeam answers (answerID, answer) grp team =

  case answer of
    AnswerGroupPosition g pos (drawID, mTeam) _ ->
      if g == grp
        then
          setGroupPosition answers answerID g pos (drawID, mTeam) team
        else
          answers

    AnswerGroupBestThirds bts points ->
      setBestThirds_ answers answerID bts grp team

    _ -> -- catch all
      answers

{- @@@ -}
setGroupPosition : Answers -> AnswerID -> Group -> Pos -> Draw -> Team -> Answers
setGroupPosition answers answerID grp pos (drawID, mTeam) team =
  let
    (newTeam, toReset) =
      case mTeam of
        Just t ->
          if (t == team)
            then
              (Nothing, [team])
            else
              (Just team, [team, t])
        Nothing ->
          (Just team, [team])

    cleanAnswers =
      unsetTeams answers (Just I) toReset

    newAnswers =
      updateMatchWinner drawID newTeam cleanAnswers

    newDraw =
      (drawID, newTeam)

    newAnswer =
      answerID => AnswerGroupPosition grp pos newDraw Nothing

  in
    setAnswer newAnswer newAnswers


{-
  Set the topscorer answer.
-}
setTopscorer : Answers -> Answer -> Topscorer -> Answers
setTopscorer answers (answerID, answer) topscorer =
  let
    newAnswer =
      case answer of
        AnswerTopscorer _ points ->
          answerID => AnswerTopscorer topscorer points
        _ ->
          answerID => answer
  in
    setAnswer newAnswer answers


{-
  Set the participant data.
-}
setParticipant : Answers -> Answer -> Participant -> Answers
setParticipant answers (answerID, answer) participant =
  let
    newAnswer =
      case answer of
        AnswerParticipant _ ->
          answerID => AnswerParticipant participant
        _ ->
          answerID => answer
  in
    setAnswer newAnswer answers



-- ******* --
-- PRIVATE - OTHER FUNCTIONS --

-- Generic
{-
  Updates an answer in the Answers. Maps all answers, but only updates
  the answer if it has the same id.
-}
updateAnswer : Answer -> Answer -> Answer
updateAnswer newAnswer currentAnswer =
  if (Tuple.first newAnswer) == (Tuple.first currentAnswer)
    then
      newAnswer
    else
      currentAnswer

setAnswer : Answer -> Answers -> Answers
setAnswer answer answers =
  List.map (updateAnswer answer) answers


-- Bracket functions

{-
  Remove team from a bracket answer
  - Removes the team from the match if it is in
  - Removes the team a selected match winner if it was selected
  Only removes the team from this answer if the round of the match
  is smaller larger than or equal to the round given in the input.

  Does NOT recursively remove the team from next rounds, assumes this
  function is applied as part of a map over the answers answers:

    List.map (unsetTeamAnswer round team) answers

 -}
unsetTeamAnswer : List Team -> Round -> Answer -> Answer
unsetTeamAnswer teams rnd answer =
  case answer of

    (answerID, AnswerGroupPosition g pos draw points) ->
      case (rnd, draw) of
        (I, (drawId, Just t)) ->
          if List.member t teams
            then
              (answerID, AnswerGroupPosition g pos (drawId, Nothing) points)
            else
              answer
        _ ->
          answer

    (answerID, AnswerMatchWinner r match nextID mTeam points) ->

      let
        roundToReset =
          isSameOrANextRound r rnd
      in
        if roundToReset
          then
            unsetMatchWinner teams answerID r match nextID mTeam points

          else
            answer


    (answerID, AnswerBracket bracket points) ->
      let
        newBracket =
          List.map Just teams
          |> List.foldr (flip B.unsetQualifier) bracket

        newAnswer =
          (answerID, AnswerBracket newBracket points)
      in
        newAnswer
    _ ->
      answer

unsetMatchWinner : List Team -> AnswerID -> Round -> Match -> NextID -> Maybe Team -> Points -> Answer
unsetMatchWinner teams answerId r match nextID mTeam points =
  let
    newMatch =
      unsetTeamsMatch match teams

    newAnswer mTm =
      (answerId, AnswerMatchWinner r newMatch nextID mTm points)

  in
    case mTeam of
      Nothing ->
        newAnswer Nothing

      Just t ->
        if List.member t teams
          then
            newAnswer Nothing
          else
            newAnswer mTeam

unsetTeamsMatch : Match -> List Team -> Match
unsetTeamsMatch match teams =
  List.foldr (flip unsetTeamMatch) match teams
{-  case teams of
    (t::rest) ->
      let
        newMatch =
          unsetTeamMatch match t
      in
        unsetTeamsMatch newMatch rest
    [] ->
      match
-}
unsetTeams : Answers -> Maybe Round -> List Team -> Answers
unsetTeams answers mRound teams =
  case mRound of
    Nothing ->
      answers
    Just r ->
      List.map (unsetTeamAnswer teams r) answers


{-
  Updating the bracket.
-}
setDrawMatchWinner : Draw -> Answer -> Answer
setDrawMatchWinner ((drawID, mTeam) as draw) (answerID, answer) =
  let
    nextTeam mTm match =
      case mTm of
        Nothing ->
          Nothing
        Just t ->
          if (List.member t (teamsInMatch match))
            then
              Just t
            else
              Nothing

  in
    case answer of

      AnswerMatchWinner rnd match nextID mTm points ->
        let
          newMatch =
            setTeamMatch match draw

          newSelected =
            nextTeam mTm newMatch

          newAnswer =
            AnswerMatchWinner rnd newMatch nextID newSelected points
        in
          answerID => newAnswer

      AnswerBracket bracket points ->
        let
          newBracket =
            B.set bracket drawID mTeam

          newAnswer =
            AnswerBracket newBracket points
        in
          answerID => newAnswer

      _ ->
        answerID => answer


{-
  Toplevel function for assigning a team to a slot in the bracket.
  This maps over all answers. One of these will be updated.
-}
setDraw : Draw -> Answers -> Answers
setDraw draw answers =
  List.map (setDrawMatchWinner draw) answers

setDraws : List Draw -> Answers -> Answers
setDraws draws answers =
  case draws of
    [] ->
      answers
    (draw::rest) ->
      let
        newAnswers = setDraw draw answers
      in
        setDraws rest newAnswers

setNext : NextID -> Team -> Answers -> Answers
setNext nextID team answers =
  case nextID of
    Just drawID ->
      setDraw (drawID, (Just team)) answers
    _ ->
      answers

updateMatchWinner : DrawID -> Maybe Team -> Answers -> Answers
updateMatchWinner drawID mTeam answers =
  setDraw (drawID, mTeam) answers

updateBracket : DrawID -> Maybe Team -> Answers -> Answers
updateBracket drawID mTeam answers =
  setDraw (drawID, mTeam) answers

{-
  Set the best thirds, clean the rest of the answers if anything changes, proceed
  the best thirds to the next round (first round in the bracket.)
-}

setBestThirds : Answers -> AnswerID -> BestThirds -> Group -> Team -> Answers
setBestThirds answers answerID oldBestThirds newgroup team =
  let
    newBestThirds =
      Bets.Types.BestThirds.updateChoices newgroup team oldBestThirds

    newAnswer =
      answerID => AnswerGroupBestThirds newBestThirds Nothing

    newAnswers =
      updateBracketAnswerBestThirds answers newBestThirds
  in
    setAnswer newAnswer newAnswers

updateBracketAnswerBestThirds : Answers -> BestThirds -> Answers
updateBracketAnswerBestThirds answers newBestThirds =
  let

    takenSlots =
      List.map (\(_, t, d) -> (d, Just t)) newBestThirds

    takenSlotIds =
      List.map Tuple.first takenSlots

    openSlots =
      List.filter (\s -> not (List.member s takenSlotIds)) ["T1", "T2", "T3", "T4"]
      |> List.map (\s -> (s, Nothing))

    allSlots =
      List.append takenSlots openSlots

    mBrktAnswer =
      getAnswer answers "br"

  in
    case mBrktAnswer of
      Just (bracketAnswerId, AnswerBracket brkt points) ->
        let
          newBrkt =
            B.setBulk brkt allSlots
          newAnswer =
            (bracketAnswerId, AnswerBracket newBrkt points)
        in
          setAnswer newAnswer answers

      _ ->
        answers







setBestThirds_ : Answers -> AnswerID -> BestThirds -> Group -> Team -> Answers
setBestThirds_ answers answerID oldBestThirds newgroup team =
  let

    newBestThirds =
      Bets.Types.BestThirds.updateChoices newgroup team oldBestThirds

    teamsToReset  =
      Bets.Types.BestThirds.teamsToReset oldBestThirds newBestThirds

    draws =
      List.map (\(_, t, d) -> (d, Just t)) newBestThirds

    newAnswer =
      answerID => AnswerGroupBestThirds newBestThirds Nothing

    cleanAnswers =
      teamsToReset -- get teams to be removed from bracket
      |> unsetTeams answers (Just II) -- remove teams from bracket
      |> setAnswer newAnswer -- set new Answer for BestThird


    allSet =
      (List.length newBestThirds) == 4
  in
    if allSet  -- if the list of BestThirds is full
      then
        setDraws draws cleanAnswers  -- proceed the selected bestThirds into the bracket
      else
        cleanAnswers

{-
  Toplevel function for setting the BestThird answer. The real logic is
  in setBestThirds.
-}
setBestThird : Answers -> Answer -> Group -> Team -> Answers
setBestThird answers (aid, answer) grp team =
  case answer of
    AnswerGroupBestThirds bestThirds _ ->
      setBestThirds answers aid bestThirds grp team
    _ ->
      answers


{-

-}
setWinner : Answers -> Answer -> Slot -> Winner -> Answers
setWinner answers (aid, answer) slot homeOrAway =
  case answer of
    AnswerBracket bracket points ->
      let
        newBracket =
          B.proceed bracket slot homeOrAway
        newAnswer =
          aid => AnswerBracket newBracket points
        newAnswers =
          setAnswer newAnswer answers
      in
        newAnswers
    _ ->
      answers


isMatchScoreAnswer : Answer -> Bool
isMatchScoreAnswer (_, answer) =
  case answer of
    AnswerGroupMatch _ _ _ _ ->
      True

    _ ->
      False


isGroupMatchScoreAnswer : Group -> Answer -> Bool
isGroupMatchScoreAnswer grp (_, answer) =
  case answer of
    AnswerGroupMatch g _ _ _ ->
      g == grp

    _ ->
      False


isGroupPositionAnswer : Group -> Answer -> Bool
isGroupPositionAnswer grp (_, answer) =
  case answer of
    AnswerGroupPosition g _ _ _ ->
      g == grp
    _ ->
      False


findGroupAnswers : (Answer -> Bool) -> Answers -> Answers
findGroupAnswers fltr answers =
  List.filter fltr answers

findGroupMatchAnswers : Group -> Answers -> Answers
findGroupMatchAnswers grp answers =
  findGroupAnswers (isGroupMatchScoreAnswer grp) answers


findAllGroupMatchAnswers :  Answers -> Answers
findAllGroupMatchAnswers  answers =
  findGroupAnswers  isMatchScoreAnswer answers

findGroupPositionAnswers : Group -> Answers -> Answers
findGroupPositionAnswers grp answers =
  findGroupAnswers (isGroupPositionAnswer grp) answers


encode: Answers -> Json.Encode.Value
encode answers =
  let
    d =
      List.map Bets.Types.Answer.encodeAnswer answers
  in
    Json.Encode.object d

-- map : (a -> b) -> Decoder a -> Decoder b
decode : Decoder Answers
decode =
  Json.Decode.dict Bets.Types.Answer.decode
  |> Json.Decode.map Dict.toList
  --|> Json.Decode.map (\answers -> (List.map (\(answerId, answer) -> answer) answers))
