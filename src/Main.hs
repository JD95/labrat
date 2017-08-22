{-# LANGUAGE OverloadedStrings, TypeApplications #-}
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

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

triangle :: [(Int, Float)]
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
  shader <- GL.glCreateShader ty
  let n = nullPtr @GL.GLint
  GL.glShaderSource shader 1 src n
  GL.glCompileShader shader
  let b = nullPtr @GL.GLint
  GL.glGetShaderiv shader GL.GL_COMPILE_STATUS b
  -- TODO: CHECK FOR COMPILATION ERRORS
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
  newCAString $ "#version 430"
             <> "layout(location = 0) in vec4 vPosition;\n"
             <> "layout(location = 1) in vec4 vColor;\n"
             <> "out vec4 color;\n"
             <> "void main() {\n"
             <> "  color = vec4(1.0, 1.0, 1.0, 1.0);\n"
             <> "  gl_position = vPosition;}"
             
fragShader :: IO CString
fragShader =
  newCAString $ "#version 430"
             <> "in vec4 color;\n"
             <> "out vec4 fColor;\n"
             <> "void main() {\n"
             <> "  fColor = vec4(0.0, 1.0, 0.0, 1.0);\n"             

game :: IO ()
game = do
  print "More Running!"
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  let openGLProfile = SDL.Core SDL.Debug 4 3 
  window <- SDL.createWindow  "SDL / OpenGL Example" $
            SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight,
                               SDL.windowOpenGL = Just (SDL.defaultOpenGL {SDL.glProfile = openGLProfile })}
  SDL.showWindow window

  print "Creating GL Context!"
 
  renderer  <- SDL.glCreateContext window
  SDL.glMakeCurrent window renderer

  print "Creating VAO"
  let vao = nullPtr @ GL.GLuint
  GL.glGenVertexArrays 1 vao
  vaoId <- peek vao
  print $ "vao id is: " <> show vaoId
  GL.glBindVertexArray =<< peek vao

  print "Creating VBO"
  let vbo = nullPtr @ GL.GLuint
  GL.glGenBuffers 1 vbo
  GL.glBindBuffer GL.GL_ARRAY_BUFFER =<< peek vbo

  print "Allocating Vertex Data"
  verts <- newArray @StorableArray (0,10) (0.0 :: Float)
  forM_ triangle $ \(i,e) -> writeArray verts i e

  print "Assigning Vertex data to Buffer"
  withStorableArray verts $ \vertsPtr ->
    GL.glBufferData GL.GL_ARRAY_BUFFER (CPtrdiff . fromIntegral $ sizeOf (0 :: Float)) vertsPtr GL.GL_STATIC_DRAW

  print "Creating Vertex Shader"
  vsShader <- vertexShader
  vsPtr <- malloc @ (Ptr GL.GLchar)
  poke vsPtr vsShader

  print "Compiling Vertex Shader"
  vs <- compileShader vsPtr GL.GL_VERTEX_SHADER

  print "Creating Fragment Shader"
  fgShader <- fragShader
  fgPtr <- malloc @ (Ptr GL.GLchar)
  poke vsPtr fgShader

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
        GL.glClearColor 0.3 0.3 0.3 1.0
        GL.glClear GL.GL_COLOR_BUFFER_BIT

        GL.glDrawArrays GL.GL_TRIANGLES 0 3

        SDL.glSwapWindow window

        unless quit loop

  loop

  SDL.destroyWindow window
  SDL.quit
