{-# LANGUAGE CApiFFI #-}

module Haskript.GraphicsUI.TFD.Internal.FFI (
    c_tinyfd_version,
    c_tinyfd_requirements,
    c_tinyfd_verbose,
    c_tinyfd_silent,
    c_tinyfd_allow_curses_dialogs,
    c_tinyfd_force_console,
    c_tinyfd_response_buffer,
    c_beep,
    c_notify_popup,
    c_message_box,
    c_input_box,
    c_save_file_dialog,
    c_open_file_dialog,
    c_select_folder_dialog,
    c_color_chooser,
)
where

import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..), CUChar)
import Foreign.Ptr (Ptr)

-- Main program functions
foreign import capi unsafe "tinyfiledialogs.h &tinyfd_version"
    c_tinyfd_version :: Ptr CString
foreign import capi unsafe "tinyfiledialogs.h &tinyfd_needs"
    c_tinyfd_requirements :: Ptr CString
foreign import capi unsafe "tinyfiledialogs.h &tinyfd_verbose"
    c_tinyfd_verbose :: Ptr CInt
foreign import capi unsafe "tinyfiledialogs.h &tinyfd_silent"
    c_tinyfd_silent :: Ptr CInt
foreign import capi unsafe "tinyfiledialogs.h &tinyfd_allowCursesDialogs"
    c_tinyfd_allow_curses_dialogs :: Ptr CInt
foreign import capi unsafe "tinyfiledialogs.h &tinyfd_forceConsole"
    c_tinyfd_force_console :: Ptr CInt
foreign import capi unsafe "tinyfiledialogs.h &tinyfd_response"
    c_tinyfd_response_buffer :: Ptr CString

-- Main program functions.
foreign import capi safe "tinyfiledialogs.h tinyfd_beep" c_beep :: IO ()
foreign import capi safe "tinyfiledialogs.h tinyfd_notifyPopup"
    c_notify_popup ::
        CString
        -> CString
        -> CString
        -> IO CInt
foreign import capi safe "tinyfiledialogs.h tinyfd_messageBox"
    c_message_box ::
        CString
        -> CString
        -> CString
        -> CString
        -> CInt
        -> IO CInt
foreign import capi safe "tinyfiledialogs.h tinyfd_inputBox"
    c_input_box ::
        CString
        -> CString
        -> CString
        -> IO CString
foreign import capi safe "tinyfiledialogs.h tinyfd_saveFileDialog"
    c_save_file_dialog ::
        CString
        -> CString
        -> CInt
        -> Ptr CString
        -> CString
        -> IO CString
foreign import capi safe "tinyfiledialogs.h tinyfd_openFileDialog"
    c_open_file_dialog ::
        CString
        -> CString
        -> CInt
        -> Ptr CString
        -> CString
        -> CInt
        -> IO CString
foreign import capi safe "tinyfiledialogs.h tinyfd_selectFolderDialog"
    c_select_folder_dialog ::
        CString
        -> CString
        -> IO CString
foreign import capi safe "tinyfiledialogs.h tinyfd_colorChooser"
    c_color_chooser ::
        CString
        -> CString
        -> Ptr CUChar
        -> Ptr CUChar
        -> IO CString
