module Haskript.GraphicsUI.TFD.Internal.GlobalMutex (tFDLock) where

import Control.Concurrent.MVar (
    MVar,
    newMVar,
 )
import System.IO.Unsafe (unsafePerformIO)

{-# NOINLINE tFDLock #-}
tFDLock :: MVar ()
tFDLock = unsafePerformIO $ newMVar ()
