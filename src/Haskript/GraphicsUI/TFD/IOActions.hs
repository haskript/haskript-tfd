{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE CApiFFI #-}

module Haskript.GraphicsUI.TFD.IOActions (
    beep,
    TFDNotifyIcon (..),
    notifyPopup,
    TFDMBDialogType (..),
    TFDMBIcon (..),
    TFDMBButton (..),
    unsanitizedMessageBox,
) where

import Foreign.C.String (
    CString,
    withCString,
 )
import Foreign.C.Types (
    CInt (..),
 )

-- CInt

foreign import capi safe "tinyfiledialogs.h tinyfd_beep" c_beep :: IO ()

-- | Triggers system beep.
beep :: IO ()
beep = c_beep

foreign import capi safe "tinyfiledialogs.h tinyfd_notifyPopup"
    c_notify_popup :: CString -> CString -> CString -> IO CInt

-- | Notify icon sum type constructors to avoid raw text.
data TFDNotifyIcon
    = TFDNotifyInfo
    | TFDNotifyWarning
    | TFDNotifyError
    deriving (Eq, Show, Enum, Bounded)

-- | Triggers system notification. Does nothing on newer MacOS systems.
notifyPopup ::
    String
    -- ^ Title
    -> String
    -- ^ Message
    -> TFDNotifyIcon
    -- ^ Notification Icon, TFDNotifyInfo, TFDNotifyError
    -> IO Int
notifyPopup title message iconType =
    withCString title \cTitle ->
        withCString message \cMessage ->
            withCString convertedIconType \cIconType ->
                fromIntegral <$> c_notify_popup cTitle cMessage cIconType
  where
    convertedIconType = case iconType of
        TFDNotifyInfo -> "info"
        TFDNotifyWarning -> "warning"
        TFDNotifyError -> "error"

foreign import capi safe "tinyfiledialogs.h tinyfd_messageBox"
    c_message_box ::
        CString
        -> CString
        -> CString
        -> CString
        -> CInt
        -> IO CInt

-- | Message box dialogue type constructors to avoid raw text.
data TFDMBDialogType
    = TFDMBOk
    | TFDMBOkCancel
    | TFDMBYesNo
    | TFDMBYesNoCancel
    deriving (Eq, Show, Enum, Bounded)

-- | Message box icon type constructors to avoid raw text.
data TFDMBIcon
    = TFDMBInfo
    | TFDMBWarning
    | TFDMBError
    | TFDMBQuestion
    deriving (Eq, Show, Enum, Bounded)

-- | Message box button constructors to avoid raw text.
data TFDMBButton
    = TFDMBCancelNo
    | TFDMBOkYes
    | TFDMBNoInYesNoCancel
    deriving (Eq, Show, Enum, Bounded)

{- | Call to TFD's message box. Note that this does NOT sanitize and use of user inputs without
sanitization is unsafe.
-}
unsanitizedMessageBox ::
    String
    -- ^ Title
    -> String
    -- ^ Message
    -> TFDMBDialogType
    -- ^ Dialog type, TFDMBOk, TFDMBOkCancel, TFDMBYesNo, TFDMBYesNoCancel
    -> TFDMBIcon
    -- ^ Icon type, TFDMBInfo, TFDMBWarning, TFDMBError, TFDMBQuestion
    -> TFDMBButton
    -- ^ Default return. Choose between TFDMBCancelNo, TFDMBOkYes, TFDBNoInYesNoCancel
    -> IO (Int `Either` TFDMBButton)
    -- ^ On failure, returns the error code.
unsanitizedMessageBox title message dialogType iconType defaultButton =
    withCString title \cTitle ->
        withCString message \cMessage ->
            withCString convertedDialogType \cDialogType ->
                withCString convertedIconType \cIconType ->
                    convertToEither <$> c_message_box cTitle cMessage cDialogType cIconType convertedDefaultButton
  where
    convertedDialogType = case dialogType of
        TFDMBOk -> "ok"
        TFDMBOkCancel -> "okcancel"
        TFDMBYesNo -> "yesno"
        TFDMBYesNoCancel -> "yesnocancel"
    convertedIconType = case iconType of
        TFDMBInfo -> "info"
        TFDMBWarning -> "warning"
        TFDMBError -> "error"
        TFDMBQuestion -> "question"
    convertedDefaultButton = case defaultButton of
        TFDMBCancelNo -> 0
        TFDMBOkYes -> 1
        TFDMBNoInYesNoCancel -> 2
    convertToEither = \case
        0 -> Right TFDMBCancelNo
        1 -> Right TFDMBOkYes
        2 -> Right TFDMBNoInYesNoCancel
        n -> Left $ fromIntegral n
