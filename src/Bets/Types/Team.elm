module Bets.Types.Team
    exposing
        ( team
        , allTeams
        , display
        , displayFull
        , mdisplay
        , mdisplayFull
        , log
        , flag
        , flagUrl
        , initTeamData
        , isComplete
        , decode
        , encode
        , encodeMaybe
        )

import Html exposing (..)
import Html.Attributes exposing (style, src, class)
import Json.Encode
import Json.Decode exposing (Decoder, maybe)
import Maybe.Extra as M
import Bets.Types exposing (Team, TeamID, TeamData, TeamDatum, Group(..))


initTeamData : TeamData
initTeamData =
    [ -- group A
      russia
    , saoudiArabia
    , egypt
    , uruguay
      -- group B
    , portugal
    , spain
    , morocco
    , iran
      -- group C
    , france
    , australia
    , peru
    , denmark
      -- group D
    , argentina
    , iceland
    , croatia
    , nigeria
      -- group E
    , brazil
    , switzerland
    , costaRica
    , serbia
      -- group F
    , germany
    , mexico
    , sweden
    , southKorea
      -- group G
    , belgium
    , panama
    , tunisia
    , england
      -- group H
    , poland
    , senegal
    , colombia
    , japan
    ]


team : TeamID -> String -> Team
team teamID teamName =
    Team teamID teamName


display : Team -> String
display team =
    team.teamID


displayFull : Team -> String
displayFull team =
    team.teamName


mdisplayFull : Maybe Team -> String
mdisplayFull mteam =
    Maybe.map displayFull mteam
        |> Maybe.withDefault ""


mdisplay : Maybe Team -> String
mdisplay mteam =
    Maybe.map display mteam
        |> Maybe.withDefault ""


log : Team -> String
log team =
    team.teamName


flag : Maybe Team -> Html msg
flag mteam =
    let
        uri team =
            (flagUri 2) ++ team.teamID ++ ".png"

        default =
            team "xyz" ""
    in
        case mteam of
            Nothing ->
                img [ (src (uri default)) ] []

            Just t ->
                img [ (src (uri t)) ] []


flagUrl : Maybe Team -> String
flagUrl mteam =
    let
        uri team =
            (flagUri 2) ++ team.teamID ++ ".png"

        default =
            team "xyz" ""
    in
        case mteam of
            Nothing ->
                uri default

            Just t ->
                uri t


flagUri : Int -> String
flagUri size =
    if (size > 5) || (size < 0) then
        "http://img.fifa.com/images/flags/3/"
    else
        "http://img.fifa.com/images/flags/" ++ (toString size) ++ "/"


isComplete : Maybe Team -> Bool
isComplete mTeam =
    M.isJust mTeam


decode : Decoder Team
decode =
    Json.Decode.map2 Team
        (Json.Decode.field "teamID" Json.Decode.string)
        (Json.Decode.field "teamName" Json.Decode.string)


encodeMaybe : Maybe Team -> Json.Encode.Value
encodeMaybe mTeam =
    case mTeam of
        Just t ->
            encode t

        Nothing ->
            Json.Encode.null


encode : Team -> Json.Encode.Value
encode team =
    Json.Encode.object
        [ ( "teamID", Json.Encode.string team.teamID )
        , ( "teamName", Json.Encode.string team.teamName )
        ]


allTeams : List Team
allTeams =
    List.map .team initTeamData



-- teams


russia : TeamDatum
russia =
    { team = (team "RUS" "Rusland")
    , players =
        [ "Igor Akinfeev"
        , "Guilherme"
        , "Yuri Lodygin"
        , "Alexei Berezutsky"
        , "Vasily Berezutsky"
        , "Sergei Ignashevich"
        , "Dmitry Kombarov"
        , "Roman Neustädter"
        , "Georgy Shchennikov"
        , "Roman Shishkin"
        , "Igor Smolnikov"
        , "Igor Denisov"
        , "Dmitri Torbinski"
        , "Denis Glushakov"
        , "Alexander Golovin"
        , "Oleg Ivanov"
        , "Pavel Mamaev"
        , "Alexander Samedov"
        , "Oleg Shatov"
        , "Roman Shirokov"
        , "Artyom Dzyuba"
        , "Alexander Kokorin"
        , "Fyodor Smolov"
        ]
    , group = A
    }


egypt : TeamDatum
egypt =
    { team = { teamID = "EGY", teamName = "Egypte" }
    , players =
        [ "Hossam Hassan"
        ]
    , group = A
    }


saoudiArabia : TeamDatum
saoudiArabia =
    { team = { teamID = "KSA", teamName = "Saoedi Arabië" }
    , players =
        [ "Luis Suarez"
        ]
    , group = A
    }


uruguay : TeamDatum
uruguay =
    { team = { teamID = "URU", teamName = "Uruguay" }
    , players =
        [ "Luis Suarez"
        ]
    , group = A
    }



-- group B


portugal : TeamDatum
portugal =
    { team = (team "POR" "Portugal")
    , players =
        [ "Rui Patricio"
        , "Anthony Lopes"
        , "Eduardo"
        , "Vieirinha"
        , "Raphael Guerreiro"
        , "Cédric Soares"
        , "Eliseu"
        , "Bruno Alves"
        , "José Fonte"
        , "Ricardo Carvalho"
        , "Pepe"
        , "William Carvalho"
        , "Danilo Pereira"
        , "João Moutinho"
        , "Adrien Silva"
        , "João Mario"
        , "André Gomes"
        , "Renato Sanches"
        , "Cristiano Ronaldo"
        , "Eder"
        , "Nani"
        , "Ricardo Quaresma"
        , "Rafa Silva"
        ]
    , group = B
    }


spain : TeamDatum
spain =
    { team = (team "ESP" "Spanje")
    , players =
        [ "Iker Casillas"
        , "David De Gea"
        , "Sergio Rico"
        , "Sergio Ramos"
        , "Gerard Piqué"
        , "Dani Carvajal"
        , "Jordi Alba"
        , "Marc Bartra"
        , "Cesar Azpilicueta"
        , "Mikel San José"
        , "Juanfran"
        , "Bruno Soriano"
        , "Sergio Busquets"
        , "Koke"
        , "Thiago"
        , "Andrés Iniesta"
        , "Isco"
        , "David Silva"
        , "Pedro"
        , "Cesc Fabregas"
        , "Saúl"
        , "Aritz Aduriz"
        , "Nolito"
        , "Alvaro Morata"
        , "Lucas Vázquez"
        ]
    , group = B
    }


morocco : TeamDatum
morocco =
    { team = { teamID = "MAR", teamName = "Marokko" }
    , players =
        [ "Morrocan Striker"
        ]
    , group = B
    }


iran : TeamDatum
iran =
    { team = { teamID = "IRN", teamName = "Iran" }
    , players =
        [ "Alireza Jahanbaksh"
        ]
    , group = B
    }



-- group C


france : TeamDatum
france =
    { team = { teamID = "FRA", teamName = "Frankrijk" }
    , players =
        [ "Hugo Lloris"
        , "Steve Mandanda"
        , "Benoit Costil"
        , "Bacary Sagna"
        , "Christophe Jallet"
        , "Patrice Evra"
        , "Raphaël Varane"
        , "Laurent Koscielny"
        , "Eliaquim Mangala"
        , "Jérémy Mathieu"
        , "Lucas Digne"
        , "Lassana Diarra"
        , "N’Golo Kanté"
        , "Blaise Matuidi"
        , "Paul Pogba"
        , "Yohan Cabaye"
        , "Moussa Sissoko "
        , "Antoine Griezmann"
        , "André-Pierre Gignac"
        , "Anthony Martial"
        , "Dimitri Payet"
        , "Kingsley Coman"
        , "Olivier Giroud"
        ]
    , group = C
    }


australia : TeamDatum
australia =
    { team = { teamID = "AUS", teamName = "Australië" }
    , players =
        [ "Bret Holman"
        ]
    , group = C
    }


peru : TeamDatum
peru =
    { team = { teamID = "PER", teamName = "Peru" }
    , players =
        [ "Farfan"
        ]
    , group = C
    }


denmark : TeamDatum
denmark =
    { team = { teamID = "DEN", teamName = "Denemarken" }
    , players =
        [ "Christian Eriksen"
        ]
    , group = C
    }



-- group D


argentina : TeamDatum
argentina =
    { team = { teamID = "ARG", teamName = "Argentinë" }
    , players =
        [ "Lionel Messi"
        ]
    , group = D
    }


iceland : TeamDatum
iceland =
    { team = (team "ISL" "IJsland")
    , players =
        [ "Hannes Halldorsson"
        , "Ögmundur Kristinsson"
        , "Ingvar Jonsson"
        , "Birkir Mar Saevarsson"
        , "Ragnar Sigurdsson"
        , "Kari Arnason"
        , "Ari Freyr Skulason"
        , "Haukur Heidar Hauksson"
        , "Sverrir Ingi Ingason"
        , "Hördur Björgvin Magnusson"
        , "Hjörtur Hermannsson"
        , "Aron Einar Gunnarsson"
        , "Emil Hallfredsson"
        , "Birkir Bjarnason"
        , "Johann Berg Gudmundsson"
        , "Gylfi Sigurdsson"
        , "Theodor Elmar Bjarnason"
        , "Runar Mar Sigurjonsson"
        , "Arnor Ingvi Traustason"
        , "Eidur Gudjohnsen"
        , "Kolbeinn Sigthorsson"
        , "Alfred Finnbogason"
        , "Jon Dadi Bödvarsson"
        ]
    , group = D
    }


croatia : TeamDatum
croatia =
    { team = (team "CRO" "Kroatië")
    , players =
        [ "Danijel Subasic"
        , "Lovre Kalinic"
        , "Ivan Vargic"
        , "Dominik Livakovic"
        , "Darijo Srna"
        , "Vedran Corluka"
        , "Domagoj Vida"
        , "Ivan Strinic"
        , "Gordon Schildenfeld"
        , "Sime Vrsaljko"
        , "Tin Jedvaj"
        , "Duje Caleta-Car"
        , "Luka Modric"
        , "Ivan Rakitic"
        , "Ivan Perisic"
        , "Mateo Kovacic"
        , "Milan Badelj"
        , "Marcelo Brozovic"
        , "Alen Halilovic"
        , "Domagoj Antolic"
        , "Marko Rog"
        , "Ante Coric"
        , "Mario Mandzukic"
        , "Nikola Kalinic"
        , "Andrej Kramaric"
        , "Marko Pjaca"
        , "Duje Cop"
        ]
    , group = D
    }


nigeria : TeamDatum
nigeria =
    { team = { teamID = "NGA", teamName = "Nigeria" }
    , players =
        [ "Ochekubwu"
        ]
    , group = D
    }



-- group E


brazil : TeamDatum
brazil =
    { team = { teamID = "BRA", teamName = "Brazilië" }
    , players =
        [ "Neymar"
        ]
    , group = E
    }


switzerland : TeamDatum
switzerland =
    { team = (team "SUI" "Zwitserland")
    , players =
        [ "Roman Bürki"
        , "Marwin Hitz"
        , "Yvon Mvongo"
        , "Yann Sommer"
        , "Johan Djourou"
        , "Nicol Elvedi"
        , "Michael Lang"
        , "Stephan Lichtsteiner"
        , "François Moubandje"
        , "Ricardo Rodriguez"
        , "Fabian Schär"
        , "Philippe Senderos"
        , "Steve Von Bergen"
        , "Silvan Widmer"
        , "Valon Behrami"
        , "Eeren Derdiyok"
        , "Blerim Dzemaili"
        , "Breel Embolo"
        , "Gelson Fernandes"
        , "Fabian Frei"
        , "Admir Mehmedi"
        , "Haris Seferovic"
        , "Xherdan Shaqiri"
        , "Renato Steffen"
        , "Shani Tarashaj"
        , "Granit Xhaka"
        , "Denis Zakaria"
        , "Luca Zuffi"
        ]
    , group = E
    }


costaRica : TeamDatum
costaRica =
    { team = { teamID = "CRC", teamName = "Costa Rica" }
    , players =
        [ "Ledezma"
        ]
    , group = E
    }


serbia : TeamDatum
serbia =
    { team = { teamID = "SRB", teamName = "Servië" }
    , players =
        [ "Milosevic"
        ]
    , group = E
    }



-- group F


germany : TeamDatum
germany =
    { team = (team "GER" "Duitsland")
    , players =
        [ "Manuel Neuer"
        , "Bernd Leno"
        , "Marc-André Ter Stegen"
        , "Jérôme Boateng"
        , "Emre Can"
        , "Jonas Hector"
        , "Benedikt Höwedes"
        , "Mats Hummels"
        , "Shkodran Mustafi"
        , "Sebastian Rudy"
        , "Antonio Rüdiger"
        , "Julian Brandt"
        , "Julian Draxler"
        , "Mario Götze"
        , "Sami Khedira"
        , "Joshua Kimmich"
        , "Toni Kroos"
        , "Thomas Muller"
        , "Mesut Özil"
        , "Bastian Schweinsteiger"
        , "Julian Weigl"
        , "Karim Bellarabi"
        , "Mario Gomez"
        , " Lukas Podolski"
        , "Marco Reus"
        , "Leroy Sané"
        , "André Schürrle"
        ]
    , group = F
    }


mexico : TeamDatum
mexico =
    { team = { teamID = "MEX", teamName = "Mexico" }
    , players =
        [ "Lozano"
        ]
    , group = F
    }


sweden : TeamDatum
sweden =
    { team = (team "SWE" "Zweden")
    , players =
        [ "Patrick Carlgren"
        , "Andreas Isaksson"
        , "Robin Olsen"
        , "Ludwig Augustinsson"
        , "Andreas Granqvist"
        , "Pontus Jansson"
        , "Erik Johansson"
        , "Mikael Lustig"
        , "Victor Lindelöf"
        , "Martin Olsson"
        , "Jimmy Durmaz"
        , "Albin Ekdal"
        , "Emil Forsberg"
        , "Oscar Hiljemark"
        , "Kim Källström"
        , "Sebastian Larsson"
        , "Oscar Lewicki"
        , "Pontus Wernbloom"
        , "Erkan Zengin"
        , "Marcus Berg"
        , "John Guidetti"
        , "Zlatan Ibrahimovic"
        , "Emir Kujovic"
        ]
    , group = F
    }


southKorea : TeamDatum
southKorea =
    { team = { teamID = "KOR", teamName = "Zuid Korea" }
    , players =
        [ "Song"
        ]
    , group = F
    }



-- group G


belgium : TeamDatum
belgium =
    { team = (team "BEL" "België")
    , players =
        [ "Thibaut Courtois"
        , "Simon Mignolet"
        , "Jean-François Gillet"
        , "Jan Vertonghen"
        , "Toby Alderweireld"
        , "Nicolas Lombaerts"
        , "Thomas Vermaelen"
        , "Jason Denayer"
        , "Jordan Lukaku"
        , "Björn Engels"
        , "Dedryck Boyata"
        , "Thomas Meunier"
        , "Kevin De Bruyne"
        , "Radja Nainggolan"
        , "Moussa Dembélé"
        , "Axel Witsel"
        , "Marouane Fellaini"
        , "Dries Mertens"
        , "Eden Hazard"
        , "Romelu Lukaku"
        , "Yannick Ferreira-Carrasco"
        , "Divock Origi"
        , "Michy Batshuayi"
        , "Christian Benteke"
        ]
    , group = G
    }


panama : TeamDatum
panama =
    { team = { teamID = "PAN", teamName = "Panama" }
    , players =
        [ "Gonzalez"
        ]
    , group = G
    }


tunisia : TeamDatum
tunisia =
    { team = { teamID = "TUN", teamName = "Tunesië" }
    , players =
        [ "El Ghazi"
        ]
    , group = G
    }


england : TeamDatum
england =
    { team = { teamID = "ENG", teamName = "Engeland" }
    , players =
        [ "Joe Hart"
        , "Fraser Forster"
        , "Tom Heaton"
        , "Nathaniel Clyne"
        , "Kyle Walker"
        , "Danny Rose"
        , "Ryan Bertrand"
        , "Chris Smalling"
        , "John Stones"
        , "Gary Cahill"
        , "Dele Alli"
        , "Ross Barkley"
        , "Fabian Delph"
        , "Danny Drinkwater"
        , "Eric Dier"
        , "Jordan Henderson"
        , "James Milner"
        , "Adam Lallana"
        , "Raheem Sterling"
        , "Jack Wilshere"
        , "Andros Townsend"
        , "Wayne Rooney"
        , "Harry Kane"
        , "Jamie Vardy"
        , "Marcus Rashford"
        , "Daniel Sturridge"
        ]
    , group = G
    }



-- group H


poland : TeamDatum
poland =
    { team = (team "POL" "Polen")
    , players =
        [ "Artur Boruc"
        , "Lukasz Fabianski"
        , "Wojciech Szczesny"
        , "Przemysław Tyton (VfB Stuttgart)."
        , "Thiago Cionek"
        , "Pavel Dawidowicz"
        , "Kamil Glik"
        , "Artur Jedrzejczyk"
        , "Michał Pazdan"
        , "Lukasz Piszczek"
        , "Maciej Rybus"
        , "Bartosz Salamon"
        , "Jakub Wawrzyniak (Lechia Gdansk)."
        , "Jakub Blaszczykowski"
        , "Kamil Grosicki"
        , "Tomasz Jodlowiec"
        , "Bartosz Kapustka"
        , "Grzegorz Krychowiak"
        , "Karol Linetty"
        , "Krzysztof Maczynski"
        , "Slawomir Peszko"
        , "Filip Starzynski"
        , "Pavel Wszolek"
        , "Piotr Zielinski"
        , "Robert Lewandowski"
        , "Arek Milik"
        , "Artur Sobiech"
        , "Mariusz Stepinski"
        ]
    , group = H
    }


senegal : TeamDatum
senegal =
    { team = { teamID = "SEN", teamName = "Senegal" }
    , players =
        [ "Baobab"
        ]
    , group = H
    }


colombia : TeamDatum
colombia =
    { team = { teamID = "COL", teamName = "Colombia" }
    , players =
        [ "Carlos Valderrama"
        ]
    , group = H
    }


japan : TeamDatum
japan =
    { team = { teamID = "JPN", teamName = "Japan" }
    , players =
        [ "Honda"
        ]
    , group = H
    }
