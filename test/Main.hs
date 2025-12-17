module Main (main) where

import Data.Foldable (traverse_)

import Haskript.GraphicsUI.TFD.IOActions (
      beep
    , TFDNotifyIcon (..)
    , notifyPopup
    )

main :: IO ()
main = do
    beepTest -- beep function
    notifyTest -- Notify tests of icons.

beepTest :: IO ()
beepTest = beep

notifyTest :: IO ()
notifyTest =
    traverse_ (uncurry $ flip notifyPopup "Success!")
        [ ( "Info", TFDNotifyInfo )
        , ( "Warning", TFDNotifyWarning )
        , ( "Error", TFDNotifyError )
        ]
