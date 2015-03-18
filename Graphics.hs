module Graphics
  ( Colour
  , black
  , blue
  , green
  , cyan
  , red
  , magenta
  , yellow
  , white
  , orange
  , indigo
  , violet
  , brown
  , drawLines
  ) where

import Control.Monad
import Data.Char
import Data.List
import Graphics.Rendering.OpenGL hiding (Vertex)
import Graphics.UI.GLUT hiding (Vertex)

type Colour
  = (Double, Double, Double)

black, blue, green, cyan, red, magenta, yellow, white, orange, indigo, violet,
  brown :: Colour

black   = (0, 0, 0)
blue    = (0, 0, 1.0)
green   = (0, 1.0, 0)
cyan    = (0, 1.0, 1.0)
red     = (1.0, 0, 0)
magenta = (1.0, 0, 1.0)
yellow  = (1.0, 1.0, 0)
white   = (1.0, 1.0, 1.0)
orange  = (1.0, 0.6, 0.0)
indigo  = (0.3, 0.0, 0.5)
violet  = (0.9, 0.5, 0.9) 
brown   = (0.5, 0.3, 0.1)

-- window parameters
width  = 800
height = 640

-- initial camera parameters
sx     = 50.0
sy     = 25.0
sz     = 250.0

-- movement parameters
step   = 3.0
rot    = 22.5

type Vertex
  = (Double, Double, Double) 

type ColouredLine
  = (Vertex, Vertex, Colour)

drawLines :: [ColouredLine] -> IO ()
drawLines ls
  = do
      () <- ls `seq` return ()
      _ <- getArgsAndInitialize
      initialDisplayMode $= [DoubleBuffered, WithDepthBuffer]
      w <- createWindow "LSystems"
      windowSize $= Size width height

      actionOnWindowClose   $= ContinueExectuion
      displayCallback       $= display ls
      keyboardMouseCallback $= Just keyboardMouse
      reshapeCallback       $= Just reshape
      depthFunc             $= Just Lequal

      matrixMode $= Projection
      perspective 40.0 1.0 1.0  500.0 

      matrixMode $= Modelview 0

      -- position 'camera'
      translate (Vector3 (-sx) (-sy) (-sz) :: Vector3 GLfloat)

      mainLoop


color3f r g b = color $ Color3 r g (b :: GLfloat)
vertex3f x y z = vertex $ Vertex3 x y (z :: GLfloat)

-- Redraw (on display update)
display :: [ColouredLine] -> IO ()
display ls
  = do
      clear [ColorBuffer, DepthBuffer]
      clear [ColorBuffer]
        
      preservingMatrix $ do
        translate (Vector3 (sx) (0.0) (sx) :: Vector3 GLfloat)
        renderPrimitive Lines $ mapM_ lineVertices ls

      swapBuffers

-- Draws a ColouredLine 
lineVertices :: ColouredLine -> IO ()
lineVertices ((fromX, fromY, fromZ), (toX, toY, toZ), (r, g, b))
  = do
      color $ Color3 (realToFrac r :: GLfloat)
                     (realToFrac g)
                     (realToFrac b)
      vertex $ Vertex3 (realToFrac fromX :: GLfloat)
                       (realToFrac fromY)
                       (realToFrac fromZ) 
      vertex $ Vertex3 (realToFrac toX :: GLfloat)
                       (realToFrac toY)
                       (realToFrac toZ) 


keyboardMouse :: Key -> KeyState -> Modifiers -> Position -> IO ()
keyboardMouse (Char '\ESC') Down _ _
  = leaveMainLoop
keyboardMouse (Char 'x') Down _ _
  = loadIdentity >>
    translate (Vector3 (-sx) (-sy) (-sz) :: Vector3 GLfloat) >> 
    postRedisplay Nothing
keyboardMouse (Char 's') Down _ _
  = translate (Vector3 (0.0) (step) (0.0) :: Vector3 GLfloat) >> 
    postRedisplay Nothing
keyboardMouse (Char 'd') Down _ _
  = translate (Vector3 (-step) (0.0) (0.0) :: Vector3 GLfloat) >> 
    postRedisplay Nothing
keyboardMouse (Char 'w') Down _ _
  = translate (Vector3 (0.0) (-step) (0.0) :: Vector3 GLfloat) >>
    postRedisplay Nothing
keyboardMouse (Char 'a') Down _ _
  = translate (Vector3 (step) (0.0) (0.0) :: Vector3 GLfloat) >>
    postRedisplay Nothing
keyboardMouse (Char 'q') Down _ _
  = translate (Vector3 (0.0) (0.0) (-step) :: Vector3 GLfloat) >>
    postRedisplay Nothing
keyboardMouse (Char 'e') Down _ _
  = translate (Vector3 (0.0) (0.0) (step) :: Vector3 GLfloat) >>
    postRedisplay Nothing
keyboardMouse (SpecialKey KeyLeft) Down _ _
  = do
      translate (Vector3 (sx) (0.0) (sx) :: Vector3 GLfloat)
      rotate (-rot) (Vector3 0.0 1.0 0.0 :: Vector3 GLfloat)
      translate (Vector3 (-sx) (0.0) (-sx) :: Vector3 GLfloat)
      postRedisplay Nothing
keyboardMouse (SpecialKey KeyRight) Down _ _
  = do
      translate (Vector3 (sx) (0.0) (sx) :: Vector3 GLfloat)
      rotate rot (Vector3 0.0 1.0 0.0 :: Vector3 GLfloat)
      translate (Vector3 (-sx) (0.0) (-sx) :: Vector3 GLfloat)
      postRedisplay Nothing
keyboardMouse _ _ _ _
  = return ()

-- Resize display
reshape :: Size -> IO ()
reshape s
  = do
      viewport $= (Position 0 0, s)
      postRedisplay Nothing
