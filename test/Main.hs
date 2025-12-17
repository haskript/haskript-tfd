module Main (main) where

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
    messageBoxTest -- Message boxes.

beepTest :: IO ()
beepTest = beep

-- | Note, need to install null value support since CString will avoid null pointers.
notifyTest :: IO ()
notifyTest = traverse_ go ["", "Success!"] -- need to examine null cases.
  where
    go text =
        traverse_
            (uncurry $ flip notifyPopup text)
            [ ("Info", TFDNotifyInfo)
            , ("Warning", TFDNotifyWarning)
            , ("Error", TFDNotifyError)
            ]

messageBoxTest :: IO () -- need to refactor to examine null string cases.
messageBoxTest =
    traverse_
        (((print =<<) .) $ uncurry $ uncurry $ unsanitizedMessageBox "Test" "Success!")
        messageBoxTests

-- | Container to generate all basic valid inputs for messageBoxTest.
messageBoxTests :: [((TFDMBDialogType, TFDMBIcon), TFDMBButton)]
messageBoxTests = do
    diaType <- [minBound .. maxBound]
    iconType <- [minBound .. maxBound]
    defButton <- case diaType of
        TFDMBOk -> [TFDMBOkYes]
        TFDMBOkCancel -> [TFDMBCancelNo, TFDMBOkYes]
        TFDMBYesNo -> [TFDMBCancelNo, TFDMBOkYes]
        TFDMBYesNoCancel -> [TFDMBCancelNo, TFDMBOkYes, TFDMBNoInYesNoCancel]
    [((diaType, iconType), defButton)]
