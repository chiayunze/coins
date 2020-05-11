module Interface.Draw.Exceptions where

import Brick.Types           (Padding (Max), ViewportType (Both, Horizontal), Widget)
import Brick.Widgets.Border  (borderWithLabel)
import Brick.Widgets.Center  (hCenter)
import Brick.Widgets.Core    (hBox, hLimit, padLeftRight, padRight, setAvailableSize, str, txt, vBox, vLimit, viewport,
                              withAttr)
import Brick.Widgets.List    (list, renderList)
import Core.Exceptions       (exceptionMessage, exceptionSource, exceptionTime)
import Data.List             (intersperse)
import Data.Vector           (fromList)
import Interface.Draw.Common (focusHighlight, formatTimeGMT8)
import Interface.State       (State, Tab (Excs), X (ExceptionsHeaderViewport, ExceptionsList, ExceptionsViewport),
                              exceptions)
import Interface.Theme       (cyanHeader, redText)

drawExceptions :: State -> Widget X
drawExceptions s = borderWithLabel (focusHighlight s Excs $ txt "Messages") filledWidget
    where   filledWidget = vBox [
                vLimit 1 $ viewport ExceptionsHeaderViewport Horizontal $ hLimit 250 header
              , viewport ExceptionsViewport Both $ setAvailableSize (250, height) $ renderList renderSingleException False l
              ]
            height =  if null excs then 1 else length excs
            excs = exceptions s
            l = list ExceptionsList (fromList excs) 1

            renderSingleException _ e = vLimit 1 $ padLeftRight 1 $ hBox $ intersperse (txt "  ") [
                hLimit 8 $ hCenter $ str $ formatTimeGMT8 $ exceptionTime e
              , hLimit 12 $ padRight Max $ txt $ exceptionSource e
              , withAttr redText $ txt $ exceptionMessage e
              ]

            header = withAttr cyanHeader $ padLeftRight 1 $ hBox $ intersperse (txt "  ") [
                hLimit 8 $ hCenter $ txt "Time"
              , hLimit 12 $ hCenter $ txt "Source"
              , padRight Max $ txt "Message"
              ]
