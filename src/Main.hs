{-# LANGUAGE ScopedTypeVariables #-}
module Main where
import Data.Char
import Data.Bits
import Data.Maybe
import Control.Monad
import Control.Exception
import GHC.Int
import Graphics.X11.Xlib
import Graphics.X11.Xlib.Extras
import PAM2
import System.Process
import System.Posix
import System.Directory
import System.Exit (exitWith, ExitCode(..))
import Control.Concurrent (threadDelay)

user = "jonathantanner"
testing = False
display = ":0"

main :: IO ()
main = do
  xHandle <- startX
  disp <- repeatUntilSuccess openDisplay display
  let screen = defaultScreenOfDisplay disp
      screenNo = screenNumberOfScreen screen
      width = widthOfScreen screen
      height = heightOfScreen screen
  rootWin <- rootWindow disp screenNo
  win <- mkWindow disp screen rootWin 0 0 width height
  mapWindow disp win
  updateWin disp win xHandle initialState
  exitWith ExitSuccess

repeatUntilSuccess :: (b -> IO a) -> b -> IO a
repeatUntilSuccess f x = catch (f x) (\ (_ :: SomeException) -> repeatUntilSuccess f x)

startX :: IO (ProcessHandle)
startX = (\(_, _, _, h) -> h) <$> createProcess process
  where process = CreateProcess (RawCommand "/usr/bin/X" [display])
                                 Nothing
                                 Nothing
                                 Inherit
                                 Inherit
                                 Inherit
                                 True
                                 True
                                 True
                                 True
                                 True
                                 True
                                 Nothing
                                 Nothing

initColor :: Display -> String -> IO Pixel
initColor disp color = do
  let colormap = defaultColormap disp (defaultScreen disp)
  (apros,real) <- allocNamedColor disp colormap color
  return $ color_pixel apros
   
mkWindow :: Display
         -> Screen
         -> Window
         -> Position
         -> Position
         -> Dimension
         -> Dimension
         -> IO Window
mkWindow disp screen rootWin x y w h = do
  let visual = defaultVisualOfScreen screen
      attrmask = cWOverrideRedirect .|. cWEventMask
  win <- allocaSetWindowAttributes $ \attributes -> do
    set_override_redirect attributes False
    set_event_mask attributes (exposureMask .|. buttonPressMask .|. keyPressMask)
    createWindow disp rootWin x y w h 0 (defaultDepthOfScreen screen) inputOutput visual attrmask attributes
  return win

setVisible :: Display -> ScreenNumber -> Window -> Bool -> IO ()
setVisible disp _        win True  = mapWindow disp win
setVisible disp screenNo win False = withdrawWindow disp win screenNo

drawUi :: Display -> Window -> State -> IO ()
drawUi disp win state = do
  (root, x, y, width, height, border, depth) <- getGeometry disp win
  bgcolor <- initColor disp "Midnight Blue"
  gc <- createGC disp win
  setForeground disp gc bgcolor
  fillRectangle disp win gc 0 0 width height
  drawPass disp win gc (fromIntegral (width `div` 2)) (fromIntegral (height `div` 2)) state
  font <- loadQueryFont disp "-misc-fixed-*-*-*-*-16-*-*-*-*-*-*-*"
  printString disp win gc font (fromIntegral (width `div` 2)) (fromIntegral (height `div` 2) + 100) (errorMessage state)
  freeFont disp font
  freeGC disp gc

drawPass :: Display -> Drawable -> GC -> Position -> Position -> State -> IO ()
drawPass disp d gc midX midY state = do
  font <- loadQueryFont disp "-misc-fixed-*-*-*-*-32-*-*-*-*-*-*-*"
  setFont disp gc (fontFromFontStruct font)
  let count = passwordLen state
      numSpaces = max 16 (count + 1)
      width = fromIntegral (textWidth font "*")
      (_, ascent, descent, _) = textExtents font "*"
      x = midX - fromIntegral ((((2 * numSpaces) - 1) * fromIntegral width) `div` 2)
      y = midY - ascent
      xs = map ((x +) . (2 * fromIntegral width *)) [0..fromIntegral (numSpaces - 1)]
      filleds = take numSpaces (take (count) (repeat True) ++ repeat False)
  foldM (\_ (x', filled) -> drawPassChar disp d gc x' y width ascent descent filled) () (zip xs filleds)
  freeFont disp font

drawPassChar :: Display -> Drawable -> GC -> Position -> Position -> Dimension -> GHC.Int.Int32 -> GHC.Int.Int32 -> Bool -> IO ()
drawPassChar disp d gc x y w ascent descent filled = do
  let width = fromIntegral w
      height = fromIntegral (ascent + descent)
  fgcolor <- initColor disp "black"
  bgcolor <- initColor disp "white"
  setForeground disp gc bgcolor
  fillRoundedRect disp d gc x y width height (width `div` 3)
  if filled then (do
    setForeground disp gc fgcolor
    drawString disp d gc x (y + fromIntegral ascent) "*"
    ) else (return ())

fillRoundedRect :: Display -> Drawable -> GC -> Position -> Position -> Dimension -> Dimension -> Dimension -> IO ()
fillRoundedRect disp d gc x y w h r = do
  let r' = fromIntegral r
      w' = fromIntegral w
      h' = fromIntegral h
  fillRectangles disp d gc [
    Rectangle (x + r') y (w - fromIntegral (2 * r')) h,
    Rectangle x (y + r') w (h - fromIntegral (2 * r'))
    ]
  fillArcs disp d gc [
    Arc (x + w' - r' - r') y (r + r) (r + r) 0 5760,
    Arc x y (r + r) (r + r) 5760 5760,
    Arc x (y + h' - r' - r') (r + r) (r + r) 11520 5760,
    Arc (x + w' - r' - r') (y + h' - r' - r') (r + r) (r + r) 17280 5760
    ]

printString :: Display
            -> Drawable
            -> GC
            -> FontStruct
            -> Position
            -> Position
            -> String
            -> IO ()
printString disp d gc font midX midY str = do
  setFont disp gc (fontFromFontStruct font)
  let width = textWidth font str
      (_, ascent, descent, _) = textExtents font str
      x = midX - (width `div` 2)
      y = midY - ascent
  fgcolor <- initColor disp "white"
  setForeground disp gc fgcolor
  drawString disp d gc x y str


updateWin :: Display -> Window -> ProcessHandle -> State -> IO ()
updateWin disp win xHandle state = do
  drawUi disp win state
  sync disp True
  allocaXEvent $ \e -> do
    nextEvent disp e
    ev <- getEvent e
    processEvent ev xHandle state >>= updateWin disp win xHandle
    

processEvent :: Event -> ProcessHandle -> State -> IO State
processEvent (ExposeEvent _ _ _ _ _ _ _ _ _ _)           _       = return
processEvent (ButtonEvent _ _ _ _ _ _ _ _ x y _ _ _ _ _) _       = return
processEvent (KeyEvent _ _ _ d _ _ _ _ _ _ _ _ s k _)    xHandle = fromMaybe return (pressKey xHandle <$> keyFromKeyCode k s)

data State = State String String String
password :: State -> String
password (State xs ys _) = reverse xs ++ ys
passwordLen :: State -> Int
passwordLen (State xs ys _) = length xs + length ys
errorMessage :: State -> String
errorMessage (State _ _ message) = message

initialState :: State
initialState = State "" "" ""

pressKey :: ProcessHandle -> Key -> State -> IO State
pressKey _       (Print x)        (State  xs     ys    _) = return (State (x:xs) ys "")
pressKey _        Del             (State  xs    (y:ys) _) = return (State xs ys "")
pressKey _        Backspace       (State (x:xs)  ys    _) = return (State xs ys "")
pressKey _       (Arrow LeftDir)  (State (x:xs)  ys    _) = return (State xs (x:ys) "")
pressKey _       (Arrow RightDir) (State  xs    (y:ys) _) = return (State (y:xs) ys "")
pressKey _       (Arrow UpDir)    (State  xs     ys    _) = return (State "" (revAdd xs ys) "")
pressKey _       (Arrow DownDir)  (State  xs     ys    _) = return (State (revAdd ys xs) "" "")
pressKey _        Home            (State  xs     ys    _) = return (State "" (revAdd xs ys) "")
pressKey _        End             (State  xs     ys    _) = return (State (revAdd ys xs) "" "")
pressKey xHandle  Enter            state                  = checkPass xHandle state
pressKey _        Esc              _                      = exitWith ExitSuccess
pressKey _        _                state                  = return state

checkPass :: ProcessHandle -> State -> IO State
checkPass xHandle state = do
  let conv = Conv () . const $ mapM (\(Message style msg) -> case style of
        PromptEchoOff -> print msg >> (return . password $ state)
        PromptEchoOn  -> print msg >> return user
        ErrorMsg      -> print ("Error: " ++ msg) >> return "Error"
        TextInfo      -> print ("Info: "  ++ msg) >> return "Info"
        )
  pamConv <- pamConvFromConv conv
  pamRet <- peekPamReturnT $ start "xmodm" user pamConv >>= authenticate 0 >>= acctMgmt 0 >>= setCred establish >>= openSession 0
  freePamConv pamConv
  foldPamReturn (\_ -> do
    (if not testing then openWM xHandle else print "Logging in")
    return initialState
    ) ((State "" "" <$>) . errToString) pamRet
  
openWM :: ProcessHandle -> IO ()
openWM xHandle = do
  --terminateProcess xHandle
  entry <- getUserEntryForName user
  let dir = homeDirectory entry
  setCurrentDirectory dir
  let process = CreateProcess (ShellCommand "exec /bin/bash --login xmonad")
                              (Just dir)
                              (Just [
                                ("DISPLAY", display),
                                ("HOME", dir),
                                ("PWD", dir),
                                ("SHELL", userShell entry),
                                ("USER", user),
                                ("LOGNAME", user),
                                ("PATH", "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"),
                                ("LANG", "en_GB.UTF-8")
                                ])
                              Inherit
                              Inherit
                              Inherit
                              False
                              False
                              False
                              False
                              False
                              False
                              (Just (userGroupID entry))
                              (Just (userID entry))
  (_, _, _, handle) <- createProcess process
  exitWith ExitSuccess


eitherToMaybe :: Either a () -> Maybe a
eitherToMaybe (Left  x) = Just x
eitherToMaybe (Right _) = Nothing

revAdd  []    ys = ys
revAdd (x:xs) ys = revAdd xs (x:ys)

data Side = LeftSide | RightSide
  deriving Show
data Direction = UpDir | DownDir | LeftDir | RightDir
  deriving Show
data Key = Print Char | Esc | Fn Int | PrtScr | Ins | Del | Backspace | Tab | Enter | CapsLock | Shift Side | Ctrl Side | Super | Alt Side | Arrow Direction | PgUp | PgDown | Home | End | NumLock | NumEnter
  deriving Show

mapKeyChar :: (Char -> Char) -> Key -> Key
mapKeyChar f (Print x) = Print (f x)
mapKeyChar _        x  = x

keyFromKeyCode :: KeyCode -> KeyMask -> Maybe Key
keyFromKeyCode code mask
  | mask .&. shiftMask == shiftMask = upper code
  | otherwise                       = lower code
  where lower :: KeyCode -> Maybe Key
        lower code
          | 9 == code                  = Just Esc
          | 10 <= code && code <= 18   = (Just . Print . head . show) (code - 9)
          | 19 <= code && code <= 21   = (Just . Print) ("0-=" !!! (code - 19))
          | 22 == code                 = Just Backspace
          | 23 == code                 = Just Tab
          | 24 <= code && code <= 35   = (Just . Print) ("qwertyuiop[]" !!! (code - 24))
          | 36 == code                 = Just Enter
          | 37 == code                 = Just (Ctrl LeftSide)
          | 38 <= code && code <= 49   = (Just . Print) ("asdfghjkl;'`" !!! (code - 38))
          | 50 == code                 = Just (Shift LeftSide)
          | 51 <= code && code <= 61   = (Just . Print) ("#zxcvbnm,./" !!! (code - 51))
          | 62 == code                 = Just (Shift RightSide)
          | 63 == code                 = (Just . Print) '*'
          | 64 == code                 = Just (Alt LeftSide)
          | 65 == code                 = (Just . Print) ' '
          | 66 == code                 = Just CapsLock
          | 73 == code                 = Just (fn 7)
          | 77 == code                 = Just NumLock
          | 79 <= code && code <= 91   = (Just . Print) ("789-456+1230." !!! (code - 79))
          | 94 == code                 = (Just . Print) '\\'
          | 104 == code                = Just NumEnter
          | 105 == code                = Just (Ctrl RightSide)
          | 106 == code                = (Just . Print) '/'
          | 107 == code                = Just PrtScr
          | 108 == code                = Just (Alt RightSide)
          | 110 == code                = Just Home
          | 111 == code                = Just (Arrow UpDir)
          | 112 == code                = Just PgUp
          | 113 == code                = Just (Arrow LeftDir)
          | 114 == code                = Just (Arrow RightDir)
          | 115 == code                = Just End
          | 116 == code                = Just (Arrow DownDir)
          | 117 == code                = Just PgDown
          | 118 == code                = Just Ins
          | 119 == code                = Just Del
          | 121 <= code && code <= 123 = Just (fn (code - 120))
          | 133 == code                = Just (fn 8)
          | 171 <= code && code <= 173 = Just (fn (177 - code))
          | 225 == code                = Just (fn 9)
          | 232 <= code && code <= 233 = Just (fn (code - 221))
          | otherwise                  = Nothing
        upper :: KeyCode -> Maybe Key
        upper code
          | 10 <= code && code <= 21 = (Just . Print) ("!\"£$%^&*()_+" !!! (code - 10))
          | 34 <= code && code <= 35 = (Just . Print) ("{}" !!! (code - 34))
          | 47 <= code && code <= 49 = (Just . Print) (":@¬" !!! (code - 47))
          | 51 == code               = (Just . Print) '~'
          | 59 <= code && code <= 61 = (Just . Print) ("<>?" !!! (code - 59))
          | 94 == code               = (Just . Print) '|'
          | otherwise                = mapKeyChar toUpper <$> lower code
        xs !!! n = xs !! (fromIntegral n)
        fn n = Fn (fromIntegral n)

prints :: Show a => [a] -> IO ()
prints = putStrs . map show
putStrs xs = mapM_ (\x -> putStr $ x ++ " ") xs >> putStr "\n"

