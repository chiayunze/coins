module Interface.Draw.Common where

import Brick.Types         (Widget)
import Brick.Widgets.Core  (withAttr)
import Data.Time           (UTCTime)
import Data.Time.Format    (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (hoursToTimeZone, utcToZonedTime)
import Interface.State     (State, Tab, display, focus)
import Interface.Theme     (yellowText)

focusHighlight :: State -> Tab -> Widget n -> Widget n
focusHighlight state tab = if (focus . display $ state) == tab then withAttr yellowText else id

formatTimeGMT8 :: UTCTime -> String
formatTimeGMT8 t = formatTime defaultTimeLocale "%H:%M:%S" $ utcToZonedTime (hoursToTimeZone 8) t
