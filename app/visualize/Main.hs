{-# LANGUAGE OverloadedStrings #-}

{-
  Crude OpenGL 3.1+ R-Tree visualizer.

  Controls:

  - Escape key to close the application;

  - Right arrow key to add a new bounding rectangle to the tree. Left arrow key to
    revert to the previous states;

  - M key to switch the insertion mode, displayed as a triangle in the top right
    of the window. Red triangle means insertGut is used, blue for insert. Switching
    modes resets the tree to zero elements.

  Element generation is pure, generating the same list every time.
-}

module Main where

import           Data.RTree.Internal (RTree (..), Node (..), MBR (MBR))
import qualified Data.RTree.Lazy as R

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as BSC
import           Data.ByteString.Internal (ByteString (..))
import           Data.ByteString.Unsafe
import           Data.Colour
import           Data.Colour.Names
import           Data.Colour.SRGB
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



randMBR :: RandomGen g => g -> (MBR Int, g)
randMBR g = let (a, g')  = uniformR (20, 1000) g
                (b, g'') = uniformR (20, 1000) g'
            in (MBR a b (a + 4) (b + 4), g'')



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



data State =
       State
         { sGen     :: StdGen
         , sOffset  :: Int
         , sMode    :: Bool
         , sHistory :: [RTree Int ()]
         , sFuture  :: [RTree Int ()]
         , sVao     :: GLuint
         , sVbo     :: GLuint
         }

zero :: GLuint -> GLuint -> State
zero = State (mkStdGen 0) 0 False [] []

reset :: State -> State
reset s = s { sGen     = mkStdGen 0
            , sOffset  = 0
            , sHistory = []
            , sFuture  = []
            }

forwards :: State -> State
forwards s = case sFuture s of
               []   -> let (new, g') = randMBR (sGen s)
                           alg | sMode s   = R.insert    new ()
                               | otherwise = R.insertGut new ()
                       in s { sGen = g'
                            , sHistory = case sHistory s of
                                           []   -> alg R.empty : []
                                           h:hs -> alg h : h : hs
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
    Switch     -> return (reset s { sMode = not $ sMode s }, False)
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



data Point = Point Int Int (Colour Float)
             deriving Show

flatten :: Point -> [Float]
flatten (Point x y col) =
  let RGB r g b = toSRGB col
  in [ fromIntegral x / 1024, fromIntegral y / 1024, r, g, b, 1 ]

mbr :: MBR Int -> Colour Float -> [Point]
mbr (MBR xmin ymin xmax ymax) col =
  [ Point xmin ymin col
  , Point xmin ymax col
  , Point xmin ymax col
  , Point xmax ymax col
  , Point xmax ymax col
  , Point xmax ymin col
  , Point xmax ymin col
  , Point xmin ymin col
  ]

colors :: (Floating a, Ord a) => [Colour a]
colors = cycle [lightgreen, yellow, violet, cyan, red, magenta]

visualize :: RTree Int a -> [Point]
visualize (Root ba a)  = visual 1 a <> mbr ba (colors !! 0)
  where
    visual i (Node as) = foldMap (visual $ i + 1) (snd <$> as)
                                 <> foldMap (flip mbr $ colors !! i) (fst <$> as)
    visual _ (Leaf as) = foldMap (flip mbr white) (fst <$> as)

visualize (Leaf1 ba _) = mbr ba (colors !! 0)
visualize Empty        = []


draw :: State -> IO ()
draw state = do
  let mode | sMode state = [ Point 1024 1000 blue
                           , Point 1024 1024 blue
                           , Point 1000 1024 blue
                           ]
           | otherwise   = [ Point 1024 1000 red
                           , Point 1024 1024 red
                           , Point 1000 1024 red
                           ]
  bufferData GL_ARRAY_BUFFER . VS.fromList $ foldMap flatten mode
  glDrawArrays GL_TRIANGLE_STRIP 0 3

  case sHistory state of
    []      -> return ()
    Empty:_ -> return ()
    tree:_  -> do
      let datum = visualize tree
      bufferData GL_ARRAY_BUFFER . VS.fromList $ foldMap flatten datum
      glDrawArrays GL_LINES 0 (fromIntegral $ length datum)
