{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
module Model where

import Data.Monoid
import Control.Monad
import Linear
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import Graphics.GL.Core43
import Foreign.Storable
import Data.Array.MArray
import Data.Array.Storable
import Data.Array.IO
import Data.HVect hiding (length)

import Shader

newtype Vao = Vao GLuint

data ArrayBuffer

newtype Vbo b d = Vbo GLuint

data BasicModel = BasicModel Vao (Vbo ArrayBuffer GLfloat)

basicModel :: StorableArray Int Float -> Int -> IO BasicModel
basicModel vs size = do
  print "Creating VAO"
  vao <- malloc @GLuint
  glGenVertexArrays 1 vao
  glBindVertexArray =<< peek vao
  
  print "Creating VBO"
  vbo <- malloc @GLuint
  glGenBuffers 1 vbo
  glBindBuffer GL_ARRAY_BUFFER =<< peek vbo

  print "Assigning Vertex data to Buffer"
  withStorableArray vs $ \vertsPtr ->
    let stride = CPtrdiff . fromIntegral . (*) size $ sizeOf (0 :: GLfloat)        
    in glBufferData GL_ARRAY_BUFFER stride vertsPtr GL_STATIC_DRAW

  BasicModel <$> (Vao <$> peek vao) <*> (Vbo <$> peek vbo)

