{-# LANGUAGE OverloadedStrings #-}

{-
  Crude OpenGL 3.1+ R-Tree visualizer.

  Controls:

  - Escape key to close the application;

  - Right arrow key to add a new bounding rectangle to the tree. Left arrow key to
    revert to the previous states;

  - M key to switch the insertion mode, displayed as a triangle in the top right
    of the window. Red triangle means insertGut is used, blue for insert, green for STR.
    Switching modes resets the tree to zero elements.

  Element generation is pure, generating the same list every time.
-}

module Main where

import           Data.RTree.D2.Double (RTree, MBR)
import qualified Data.RTree.D2.Double as R
import qualified Data.RTree.D2.Double.Unsafe as R

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Internal (ByteString (..))
import           Data.ByteString.Unsafe
import           Data.Colour.Names
import           Data.Colour.SRGB
import           Data.Colour.RGBSpace.HSV
import           Data.IORef
import           Data.Vector.Storable (Vector)
import qualified Data.Vector.Storable as VS
import           Foreign.ForeignPtr
import           Foreign.Marshal.Alloc
import           Foreign.Marshal.Array
import           Foreign.Ptr
import           Foreign.Storable
import           Graphics.GL
import           Graphics.UI.GLFW as GLFW
import           System.Exit
import           System.Random.Stateful



randMBR :: RandomGen g => g -> (MBR, g)
randMBR g = let (a, g')  = uniformR (20, 1000 :: Int) g
                (b, g'') = uniformR (20, 1000 :: Int) g'

                xmin = fromIntegral a
                ymin = fromIntegral b
                xmax = fromIntegral $ a + 4
                ymax = fromIntegral $ b + 4

            in (R.UnsafeMBR xmin ymin xmax ymax, g'')

list :: RandomGen g => Int -> g -> ([MBR], g)
list n g
  | n <= 0    = ([], g)
  | otherwise = let ~(bx, g')  = randMBR g
                    ~(as, g'') = list (n - 1) g'
                in (bx:as, g'')


main :: IO ()
main = do
  isInit <- GLFW.init
  when (not isInit) $ die "Cannot initialize GLFW"
  flip finally GLFW.terminate $ do
         mayWindow <- createWindow 512 512 "R-Tree Visualization" Nothing Nothing
         case mayWindow of
           Nothing     -> die "Cannot create a GLFW window"
           Just window -> setup window



data Input = Quit
           | Refresh
           | Resize Int Int
           | Switch
           | Forwards
           | Backwards

input :: Chan Input -> Key -> KeyState -> IO ()
input chan key state =
  case (key, state) of
    (Key'Escape, _                 ) -> writeChan chan Quit
    (Key'M     , KeyState'Pressed  ) -> writeChan chan Switch
    (Key'Right , KeyState'Pressed  ) -> writeChan chan Forwards
    (Key'Right , KeyState'Repeating) -> writeChan chan Forwards
    (Key'Left  , KeyState'Pressed  ) -> writeChan chan Backwards
    (Key'Left  , KeyState'Repeating) -> writeChan chan Backwards
    _                                -> return ()



setup :: Window -> IO ()
setup window = do
  chan <- newChan
  setKeyCallback           window . Just $ \_ k _ s _ -> input chan k s
  setWindowCloseCallback   window . Just $ \_ -> writeChan chan Quit
  setWindowRefreshCallback window . Just $ \_ -> writeChan chan Refresh
  setWindowSizeCallback    window . Just $ \_ x -> writeChan chan . Resize x

  doneRef <- newIORef False

  _ <- forkOS $
         bracket_ (makeContextCurrent $ Just window) (makeContextCurrent Nothing) $ do
           let loop s = do
                 (s', done) <- process chan s
                 if done
                   then writeIORef doneRef True
                   else do glClear GL_COLOR_BUFFER_BIT
                           draw s'
                           swapBuffers window
                           loop s'
           (vao, vbo) <- setupGL doneRef
           loop $ zero vao vbo

  let wait = do
        waitEvents
        done <- readIORef doneRef
        unless done wait

  wait



data Mode = Gut | BKSS | STR

modeHue :: Mode -> Float
modeHue Gut  = 0
modeHue BKSS = 210
modeHue STR  = 105

data State =
       State
         { sGen     :: StdGen
         , sOffset  :: Int
         , sMode    :: Mode
         , sHistory :: [RTree ()]
         , sFuture  :: [RTree ()]
         , sVao     :: GLuint
         , sVbo     :: GLuint
         }

zero :: GLuint -> GLuint -> State
zero = State (mkStdGen 4) 0 Gut [] []

reset :: State -> State
reset s = s { sGen     = mkStdGen 4
            , sOffset  = 0
            , sHistory = []
            , sFuture  = []
            }

forwards :: State -> State
forwards s =
  case sFuture s of
    []   ->
      case sMode s of
        Gut  -> let (new, g') = randMBR (sGen s)
                in s { sGen = g'
                     , sHistory = case sHistory s of
                                    []   -> R.insertGut new () R.empty : []
                                    h:hs -> R.insertGut new () h : h : hs
                     }

        BKSS -> let (new, g') = randMBR (sGen s)
                in s { sGen = g'
                     , sHistory = case sHistory s of
                                    []   -> R.insert new () R.empty : []
                                    h:hs -> R.insert new () h : h : hs
                     }

        STR  -> let (boxes, _)  = list (sOffset s) (sGen s)
                in s { sOffset  = sOffset s + 1
                     , sHistory = R.bulkSTR (fmap (\bx -> (bx, ())) boxes) : sHistory s
                     }

    f:fs -> s { sHistory = f : sHistory s
              , sFuture  = fs
              }

backwards :: State -> State
backwards s = case sHistory s of
                []   -> s
                h:hs -> s { sHistory = hs
                          , sFuture  = h : sFuture s
                          }



process :: Chan Input -> State -> IO (State, Bool)
process chan s = do
  new <- readChan chan
  case new of
    Quit       -> return (s, True)
    Refresh    -> return (s, False)
    Resize x y -> do case x `compare` y of
                       GT -> let offset = fromIntegral $ (x - y) `quot` 2
                             in glViewport offset 0 (fromIntegral y) (fromIntegral y)
                       EQ -> glViewport 0 0 (fromIntegral x) (fromIntegral x)
                       LT -> let offset = fromIntegral $ (y - x) `quot` 2
                             in glViewport 0 offset (fromIntegral x) (fromIntegral x)
                     return (s, False)
    Switch     -> return ( reset s { sMode = case sMode s of
                                               Gut  -> BKSS
                                               BKSS -> STR
                                               STR  -> Gut
                                   }
                         , False
                         )
    Forwards   -> return (forwards  s, False)
    Backwards  -> return (backwards s, False)



vertexShader :: [ByteString]
vertexShader =
  [ "#version 130"
  , "in  vec2 point;"
  , "in  vec4 color;"

  , "out vec4 color_;"

  , "void main () {"

  , "  color_ = color;"

  , "  gl_Position = vec4 (point * 2 - 1, 0, 1);"
  , "}"
  ]

fragmentShader :: [ByteString]
fragmentShader =
  [ "#version 130"

  , "in      vec4      color_;"

  , "out     vec4      outColor;"

  , "void main () {"
  , "  outColor = color_;"
  , "}"
  ]



shaderSource :: GLuint -> [ByteString] -> IO ()
shaderSource shader bss =
  dive bss $ \(ptrs, lens) ->
    withArray ptrs $ \ptr ->
      withArray lens $ \lenptr ->
        glShaderSource shader (fromIntegral $ length bss) ptr lenptr
  where
    dive (b:bs) f =
      let newlined = b <> "\n"
      in unsafeUseAsCStringLen newlined $ \(bsptr, n) -> do
           dive bs $ f . (\(ptrs, ns) -> (bsptr:ptrs, fromIntegral n:ns))

    dive []     f = f ([], [])

shaderInfoLog :: GLuint -> IO ByteString
shaderInfoLog shader = do
  len <- alloca $ \ptr -> do
          glGetShaderiv shader GL_INFO_LOG_LENGTH ptr
          peek ptr

  fptr <- mallocForeignPtrBytes $ fromIntegral len
  withForeignPtr fptr $
    glGetShaderInfoLog shader len nullPtr

  return $ PS (castForeignPtr fptr) 0 (fromIntegral len)



compileShader :: IORef Bool -> GLuint -> IO ()
compileShader doneRef shader = do
  glCompileShader shader
  compiled <- alloca $ \ptr -> do
                glGetShaderiv shader GL_COMPILE_STATUS ptr
                peek ptr
  when (compiled == GL_FALSE) $ do
    info <- shaderInfoLog shader
    BSC.putStrLn info
    writeIORef doneRef True
    postEmptyEvent
    error $ "Could not compile shader " <> show shader



attribLocation :: IORef Bool -> GLuint -> ByteString -> IO GLuint
attribLocation doneRef prog name = do
  loc <- BSC.useAsCString name $ glGetAttribLocation prog
  if loc == -1
    then do BSC.putStrLn name
            writeIORef doneRef True
            postEmptyEvent
            error "Invalid attribute location"
    else return $ fromIntegral loc



bufferData :: GLuint -> Vector Float -> IO ()
bufferData vbo vec =
  VS.unsafeWith vec $ \ptr ->
    let len = fromIntegral $ VS.length vec * sizeOf (undefined :: Float)
    in glBufferData vbo len (castPtr ptr) GL_DYNAMIC_DRAW



setupGL :: IORef Bool -> IO (GLuint, GLuint)
setupGL doneRef = do
  (vao, vbo) <- alloca $ \ptr -> do
                  glGenVertexArrays 1 ptr
                  vao <- peek ptr
                  glGenBuffers 1 ptr
                  vbo <- peek ptr
                  return (vao, vbo)

  vert <- glCreateShader GL_VERTEX_SHADER
  shaderSource vert vertexShader
  compileShader doneRef vert

  frag <- glCreateShader GL_FRAGMENT_SHADER
  shaderSource frag fragmentShader
  compileShader doneRef frag

  prog <- glCreateProgram
  glAttachShader prog vert
  glAttachShader prog frag
  glLinkProgram prog
  linked <- alloca $ \ptr -> do
              glGetProgramiv prog GL_LINK_STATUS ptr
              peek ptr
  when (linked == GL_FALSE) $ do
    len <- alloca $ \ptr -> do
             glGetProgramiv prog GL_INFO_LOG_LENGTH ptr
             peek ptr

    fptr <- mallocForeignPtrBytes $ fromIntegral len
    withForeignPtr fptr $
      glGetProgramInfoLog prog len nullPtr

    BSC.putStrLn $ PS (castForeignPtr fptr) 0 (fromIntegral len)
    writeIORef doneRef True
    postEmptyEvent
    error "Could not link program"

  glUseProgram prog
  glBindVertexArray vert
  glBindBuffer GL_ARRAY_BUFFER frag

  pointLoc <- attribLocation doneRef prog "point"
  colorLoc <- attribLocation doneRef prog "color"

  let float = sizeOf (undefined :: GLfloat)

  glEnableVertexAttribArray pointLoc
  glEnableVertexAttribArray colorLoc
  glVertexAttribPointer pointLoc 2 GL_FLOAT 0 (fromIntegral $ 6 * float) $ intPtrToPtr 0
  glVertexAttribPointer colorLoc 4 GL_FLOAT 0 (fromIntegral $ 6 * float) $
                                                        intPtrToPtr (IntPtr $ 2 * float)

  glEnable GL_BLEND
  glBlendFunc GL_SRC_ALPHA GL_ONE_MINUS_SRC_ALPHA

  return (vao, vbo)



data Point = Point Int Int (RGB Float)
             deriving Show

flatten :: Point -> [Float]
flatten (Point x y (RGB r g b)) =
  [ fromIntegral x / 1024, fromIntegral y / 1024, r, g, b, 1 ]

mbr :: MBR -> RGB Float -> [Point]
mbr (R.UnsafeMBR xmin ymin xmax ymax) rgb =
  let xmin_ = truncate xmin
      ymin_ = truncate ymin
      xmax_ = truncate xmax
      ymax_ = truncate ymax

  in [ Point xmin_ ymin_ rgb
     , Point xmin_ ymax_ rgb
     , Point xmin_ ymax_ rgb
     , Point xmax_ ymax_ rgb
     , Point xmax_ ymax_ rgb
     , Point xmax_ ymin_ rgb
     , Point xmax_ ymin_ rgb
     , Point xmin_ ymin_ rgb
     ]

visualize :: Mode -> RTree a -> [Point]
visualize mode = visual 0
  where
    wash i
      | i <= 0    = hsv (modeHue mode) 1 1
      | otherwise = let ~(h, s, v) = hsvView $ wash (i - 1)
                    in hsv (h + 75) s (v * 3 / 4)

    visual i n =
      let col = wash (i :: Int)
      in case n of
           R.Node2 ba a bb b ->
                visual (i + 1) a <> visual (i + 1) b
             <> mbr ba col <> mbr bb col

           R.Node3 ba a bb b bc c ->
                visual (i + 1) a <> visual (i + 1) b <> visual (i + 1) c
             <> mbr ba col <> mbr bb col <> mbr bc col

           R.Node4 ba a bb b bc c bd d ->
                visual (i + 1) a <> visual (i + 1) b <> visual (i + 1) c <> visual (i + 1) d
             <> mbr ba col <> mbr bb col <> mbr bc col <> mbr bd col

           R.Leaf2 ba _ bb _           ->
             mbr ba (toSRGB white) <> mbr bb (toSRGB white)

           R.Leaf3 ba _ bb _ bc _      ->
             mbr ba (toSRGB white) <> mbr bb (toSRGB white) <> mbr bc (toSRGB white)

           R.Leaf4 ba _ bb _ bc _ bd _ ->
             mbr ba (toSRGB white) <> mbr bb (toSRGB white) <> mbr bc (toSRGB white) <> mbr bd (toSRGB white)

           R.Leaf1 ba _                -> mbr ba (toSRGB white)
           R.Empty                     -> []


draw :: State -> IO ()
draw state = do
  let modeRGB = hsv (modeHue $ sMode state) 1 1

      mode = [ Point 1024 1000 modeRGB
             , Point 1024 1024 modeRGB
             , Point 1000 1024 modeRGB
             ]

  bufferData GL_ARRAY_BUFFER . VS.fromList $ foldMap flatten mode
  glDrawArrays GL_TRIANGLE_STRIP 0 3

  case sHistory state of
    []        -> return ()
    R.Empty:_ -> return ()
    tree:_    -> do
      let datum = visualize (sMode state) tree
      bufferData GL_ARRAY_BUFFER . VS.fromList $ foldMap flatten datum
      glDrawArrays GL_LINES 0 (fromIntegral $ length datum)
