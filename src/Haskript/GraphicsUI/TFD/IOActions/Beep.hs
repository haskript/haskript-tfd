module Haskript.GraphicsUI.TFD.IOActions.Beep (beep) where

import Control.Concurrent.MVar (withMVar)

import Haskript.GraphicsUI.TFD.Internal.FFI (c_beep)
import Haskript.GraphicsUI.TFD.Internal.GlobalMutex (tFDLock)

beep :: IO ()
beep = withMVar tFDLock $ const c_beep
