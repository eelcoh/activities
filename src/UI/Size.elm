module UI.Size exposing (..)

import Element exposing (maxWidth, percent, px)
import Types exposing (ScreenSize(..))
import Window


classifyDevice : Window.Size -> ScreenSize
classifyDevice { width } =
    if width <= 600 then
        Small

    else
        Big


bodyWidth screenSize =
    case screenSize of
        Small ->
            maxWidth (percent 95)

        Big ->
            maxWidth (percent 80)


margin screenSize =
    case screenSize of
        Small ->
            5

        Big ->
            70
