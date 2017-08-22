{-# LANGUAGE ForeignFunctionInterface #-}
module LabratCore where

import Foreign.C.Types
import Foreign.C.String

foreign import ccall safe "setup_scene" setupScene :: IO ()

foreign import ccall unsafe "game_loop" gameLoop :: IO ()
