module Main (main) where

import Control.Concurrent (forkIO, forkOS)
import Control.Monad (replicateM_)
import Data.Foldable (traverse_)

import Haskript.GraphicsUI.TFD.IOActions (
    TFDMBButton (..),
    TFDMBDialogType (..),
    TFDMBIcon (..),
    TFDNotifyIcon (..),
    beep,
    notifyPopup,
    unsanitizedMessageBox,
 )

main :: IO ()
main = do
    beepTest -- beep function
    notifyTest -- Notify tests of icons.
    --    messageBoxTest -- Message boxes.
    --
    putStrLn "Done. Press any key to end tests."

    () <$ getChar

beepTest :: IO ()
beepTest = replicateM_ 30 (forkIO $ beep)

-- | Note, need to install null value support since CString will avoid null pointers.
notifyTest :: IO ()
notifyTest = traverse_ (forkIO . go) ["", "Success!"] -- Examine null cases.
  where
    go text =
        traverse_
            (((print =<<) .) $ uncurry $ flip notifyPopup text)
            [ ("Info", TFDNotifyInfo)
            , ("Warning", TFDNotifyWarning)
            , ("Error", TFDNotifyError)
            ]

messageBoxTest :: IO ()
messageBoxTest = traverse_ (forkIO . go) [("Test", "Success")]
  where
    go mix =
        traverse_
            (((print =<<) .) $ uncurry $ uncurry $ uncurry unsanitizedMessageBox mix)
            messageBoxTests

-- | Set-ups to test null inputs.
nullStringInputs :: [(String, String)]
nullStringInputs = liftA2 (,) ["", "Test"] ["", "Success"]

-- | Container to generate all basic valid inputs for messageBoxTest.
messageBoxTests :: [((TFDMBDialogType, TFDMBIcon), TFDMBButton)]
messageBoxTests = do
    (diaType, iconType) <-
        liftA2
            (,)
            [minBound .. maxBound]
            [minBound .. maxBound]
    defButton <- case diaType of
        TFDMBOk -> [TFDMBOkYes]
        TFDMBOkCancel -> [TFDMBCancelNo, TFDMBOkYes]
        TFDMBYesNo -> [TFDMBCancelNo, TFDMBOkYes]
        TFDMBYesNoCancel -> [TFDMBCancelNo, TFDMBOkYes, TFDMBNoInYesNoCancel]
    [((diaType, iconType), defButton)]
