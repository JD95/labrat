{-# LANGUAGE OverloadedStrings, TypeApplications, ForeignFunctionInterface #-}
module Main where

import Data.Monoid
import Control.Monad
import qualified SDL
import Linear
import SDL (($=))
import Foreign.C.Types
import Foreign.Ptr
import Foreign.C.String
import Foreign.Marshal.Alloc
import qualified Graphics.GL.Core43 as GL
import Foreign.Storable
import Data.Array.MArray
import Data.Array.Storable
import Data.Array.IO

import Model

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

triangle :: [(Int, GL.GLfloat)]
triangle = zip [0..] [0.0, 0.5, 0.5, -0.5, -0.5, -0.5]

main :: IO ()
main = do
  print "Hello"
  a <- newArray @StorableArray (0,10) (0.0 :: Float)
  forM_ triangle $ \(i,e) -> writeArray a i e
  forM_ [0..5] $ print <=< readArray a
  withStorableArray a $ \ptr ->
    print ptr
  print "goodbye"


compileShader :: Ptr (Ptr GL.GLchar) -> GL.GLenum -> IO GL.GLuint
compileShader src ty = do
  print "Creating a new shader!"
  shader <- GL.glCreateShader ty
  print "Passing in the shader source"
  GL.glShaderSource shader 1 src nullPtr
  print "Compiling"
  GL.glCompileShader shader
  b <- malloc @GL.GLint
  print "Checking shader result"
  GL.glGetShaderiv shader GL.GL_COMPILE_STATUS b
  result <- peek b
  when (result == 0) $ do
    print "COMPILATION FAILED!"
    GL.glGetShaderiv shader GL.GL_INFO_LOG_LENGTH b
  logLength <- peek b 
  infoLog <- newArray @StorableArray (0,100) (CChar 0)
  withStorableArray infoLog $ \infoLogPtr ->
    GL.glGetShaderInfoLog shader 100 nullPtr infoLogPtr
  forM_ [0..99] $ putChar . castCCharToChar <=< readArray infoLog 
  pure shader


linkProgram :: GL.GLuint -> GL.GLuint -> IO GL.GLuint
linkProgram vs fs = do
  program <- GL.glCreateProgram
  GL.glAttachShader program vs
  GL.glAttachShader program fs
  GL.glLinkProgram program
  pure program

vertexShader :: IO CString
vertexShader =
  newCString $ "#version 430 core\n"
            <> "layout(location = 0) in vec4 vPosition;\n"
            <> "void main() { gl_Position = vPosition; }"

fragShader :: IO CString
fragShader =
  newCString $ "#version 430 core\n"
            <> "out vec4 fColor;\n"
            <> "void main() { fColor = vec4(0.0, 1.0, 0.0, 1.0); }\n"             

game :: IO ()
game = do
  print "More Running!"
  SDL.initialize [SDL.InitVideo]
  let openGLProfile = SDL.defaultOpenGL { SDL.glProfile = SDL.Core SDL.Debug 4 3
                                        , SDL.glColorPrecision = V4 8 8 8 1
                                        }
  window <- SDL.createWindow  "SDL / OpenGL Example" $
            SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight,
                               SDL.windowOpenGL = Just openGLProfile }

  print "Creating GL Context!"
 
  renderer  <- SDL.glCreateContext window
  SDL.glMakeCurrent window renderer

  SDL.swapInterval $= SDL.SynchronizedUpdates

  GL.glClearColor 0.3 0.3 0.3 1.0
  SDL.glSwapWindow window

  print "Allocating Vertex Data"
  verts <- newArray @StorableArray (0,6) (0.0 :: Float)
  forM_ triangle $ \(i,e) -> writeArray verts i e

  (BasicModel _ (Vbo vbo)) <- basicModel verts 6

  print "Creating Vertex Shader"
  vsShader <- vertexShader
  vsPtr <- malloc @ (Ptr GL.GLchar)
  poke vsPtr vsShader

  print "Compiling Vertex Shader"
  vs <- compileShader vsPtr GL.GL_VERTEX_SHADER

  print "Creating Fragment Shader"
  fgShader <- fragShader
  fgPtr <- malloc @ (Ptr GL.GLchar)
  poke fgPtr fgShader

  print "Compiling Fragment Shdaer"
  fs <- compileShader fgPtr GL.GL_FRAGMENT_SHADER

  print "Linking Program"
  program <- linkProgram vs fs

  print "Using Program"
  GL.glUseProgram program

  print "Shader Plumbing"
  GL.glEnableVertexAttribArray 0
  GL.glVertexAttribPointer 0 2 GL.GL_FLOAT GL.GL_FALSE 0 nullPtr

  let loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents
        let quit = elem SDL.QuitEvent $ map SDL.eventPayload events

        -- OPEN GL           
        GL.glClear GL.GL_COLOR_BUFFER_BIT
                 
        GL.glBindBuffer GL.GL_ARRAY_BUFFER vbo

        GL.glDrawArrays GL.GL_TRIANGLES 0 6

        GL.glFlush

        SDL.glSwapWindow window        

        unless quit loop

  loop

  SDL.destroyWindow window
  SDL.quit
