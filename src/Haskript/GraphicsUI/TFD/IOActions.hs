{-# LANGUAGE CApiFFI, BlockArguments #-}
module Haskript.GraphicsUI.TFD.IOActions
    ( beep
    , TFDNotifyIcon (..)
    , notifyPopup
    ) where

import Foreign.C.String
    ( withCString
    , CString
    )
import Foreign.C.Types
    ( -- CInt
    )

import Data.Coerce ( coerce )

foreign import capi "tinyfiledialogs.h tinyfd_beep" c_beep :: IO ()

-- | Triggers system beep.
beep :: IO ()
beep = c_beep

foreign import capi "tinyfiledialogs.h tinyfd_notifyPopup" c_notifyPopup :: CString -> CString -> CString -> IO Int

-- | Avoids text interface of notify icon.
data TFDNotifyIcon
    = TFDNotifyInfo
    | TFDNotifyWarning
    | TFDNotifyError

-- | Triggers system notification. Does nothing on newer MacOS systems.
notifyPopup :: String -> String -> TFDNotifyIcon -> IO Int
notifyPopup title message iconType =
    withCString title \cTitle ->
    withCString message \cMessage ->
    withCString convertedIconType \cIconType ->
        coerce <$> c_notifyPopup cTitle cMessage cIconType
  where
    convertedIconType = case iconType of
        TFDNotifyInfo -> "info"
        TFDNotifyWarning -> "warning"
        TFDNotifyError -> "error"


