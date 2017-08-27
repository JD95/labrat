module Model where

import Data.Monoid
import Control.Monad
import Linear
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import qualified Graphics.GL.Core43 as GL
import Foreign.Storable
import Data.Array.MArray
import Data.Array.Storable
import Data.Array.IO

import Shader

newtype Vao = Vao GL.GLuint


data ArrayBuffer

newtype Vbo b d = Vbo GL.GLuint
