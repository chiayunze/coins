module Interface.Theme where

import           Brick.AttrMap (AttrMap, attrMap)
import           Brick.Util    (bg, fg, on)
import qualified Graphics.Vty  as V

colourMap :: AttrMap
colourMap = attrMap V.defAttr
            [ (redText, fg V.brightRed)
            , (blueText, fg V.brightBlue)
            , (yellowText, fg V.brightYellow)
            , (greenText, fg V.brightGreen)
            , (magentaText, fg V.brightMagenta)
            , (cyanText, fg V.brightCyan)
            , (whiteText, fg V.brightWhite)
            , (bblackText, fg V.brightBlack)
            , (dwhiteText, fg V.white)
            , (dblueText, fg V.blue)
            , (redBg, bg V.brightRed)
            , (blueBg, bg V.brightBlue)
            , (yellowBg, bg V.brightYellow)
            , (greenBg, bg V.brightGreen)
            , (magentaBg, bg V.brightMagenta)
            , (cyanBg, bg V.brightCyan)
            , (blackBg, bg V.brightBlack)
            , (cyanHeader, V.black `on` V.brightCyan)
            , (blackOnGreen, V.brightBlack `on` V.brightGreen)
            , (blackOnYellow, V.black `on` V.brightYellow)
            , (whiteOnBlue, V.brightWhite `on` V.brightBlue)
            , (blackOnBlue, V.black `on` V.brightBlue)
            ]

cyanHeader = "cyanHeader"
redText = "redText"
blueText = "blueText"
yellowText = "yellowText"
greenText = "greenText"
magentaText = "magentaText"
cyanText = "cyanText"
whiteText = "whiteText"
bblackText = "bblackText"
dwhiteText = "dwhiteText"
dblueText = "dblueText"
redBg = "redBg"
blueBg = "blueBg"
yellowBg = "yellowBg"
greenBg = "greenBg"
magentaBg = "magentaBg"
cyanBg = "cyanBg"
blackBg = "blackBg"
blackOnGreen = "blackOnGreen"
blackOnYellow = "blackOnYellow"
whiteOnBlue = "whiteOnBlue"
blackOnBlue = "blackOnBlue"
