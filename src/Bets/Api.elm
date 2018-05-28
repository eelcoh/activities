module Bets.Api exposing (retrieveBet)

import Bets.Bet
import Bets.Types exposing (Bet)
import Http exposing (expectJson)
import RemoteData exposing (WebData)


-- retrieveBet : String -> (Result Http.Error Bet -> msg) -> Cmd msg
-- retrieveBet uuid handlerMsg =
--     let
--         ( vrb, url ) =
--             ( "GET", "/bets/" ++ uuid )
--         req =
--             Http.request
--                 { method = vrb
--                 , headers =
--                     [ Http.header "Content-Type" "application/json"
--                     ]
--                 , url = url
--                 , body = Http.emptyBody
--                 , expect = expectJson Bets.Bet.decode
--                 , timeout = Nothing
--                 , withCredentials = False
--                 }
--     in
--         Http.send handlerMsg req


retrieveBet : String -> (WebData Bet -> msg) -> Cmd msg
retrieveBet uuid handlerMsg =
    let
        url =
            "/bets/" ++ uuid
    in
        Http.get url Bets.Bet.decode
            |> RemoteData.sendRequest
            |> Cmd.map handlerMsg
