{-# LANGUAGE DeriveDataTypeable, NoFieldSelectors, DisambiguateRecordFields, OverloadedRecordDot, DuplicateRecordFields  #-}
{-# LANGUAGE RecordWildCards, OverloadedStrings, OverloadedRecordUpdate, RebindableSyntax  #-}
{-# LANGUAGE ViewPatterns, DeriveGeneric #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}

-- | construct a ffmpeg crop command
-- ffmpeg -i input.mp4 -ss 10 -t 30 -vf "crop=640:480:10:20" output.mp4
-- based on an interactively selected region
module Main where
import Prelude
import Turtle
import System.Environment
import System.Console.CmdArgs
import Data.Maybe
import qualified Data.Text as T
import Data.Char
import Text.Read hiding (Char)
import Graphics.Gloss
import Control.Applicative
import Graphics.Gloss.Interface.IO.Interact
import Debug.Trace
import Control.Lens hiding ((<.>))
import Data.Ord hiding (Down)
import qualified Data.List as List
import Control.Concurrent
import Control.Exception
import System.Exit (exitSuccess)
import Data.Generics.Product hiding (setField)
import qualified Data.Generics.Product
import GHC.Generics (Generic)

setField :: forall f s a. (HasField' f s a) => s -> a -> s
setField a b = Data.Generics.Product.setField @f b a

-- | for argument parsing
data FFMpegCrop = FFMpegCrop
  { input :: Maybe FilePath
  , format :: Maybe String
  , uniq :: Maybe String
  , start :: Maybe Int
  , duration :: Maybe Int
  , width :: Maybe Int
  , height :: Maybe Int
  , x :: Maybe Int
  , y :: Maybe Int
  } deriving (Show, Data, Typeable, Generic)

-- | for the gui loop and the final command
data FFMpegCrop2 = FFMpegCrop2
  { input :: FilePath
  , format :: FilePath
  , uniq :: String
  , start :: Int
  , duration :: Maybe Int
  , width :: Int
  , height :: Int
  , width0, height0 :: Int
  , x :: Int
  , y :: Int
  } deriving (Show, Generic)

c2 :: (Int, Int) -- ^ maximum (w,h)
        -> FFMpegCrop -- ^ parsed arguments
        -> FFMpegCrop2 -- ^ initial crop
c2 (w, h) FFMpegCrop{..} = FFMpegCrop2
  { input = fromJust input
  , format = fromMaybe "webm" format
  , uniq = fromMaybe "" uniq
  , start = fromMaybe 0 start
  , duration = duration
  , width = fromMaybe w width
  , height = fromMaybe h height
  , width0 = w
  , height0 = h
  , x = fromMaybe 0 x
  , y = fromMaybe 0 y
  }

ffmpegCrop = FFMpegCrop
  { input = def &= typ "INFILE" &= argPos 0,
    format = Just "webm" &= help "output file extension (webm) by default",
    uniq = Just "_cropped_%w_%h" &= help "output file suffix can contain %d for the duration of the crop in seconds, %w for the width, %h for the height, %x for the x position, %y for the y position and %% for a literal %. Default: _cropped_%w_%h",
    start = Nothing &= typ "INT" &= help "start time in seconds",
    duration = Nothing &= typ "INT" &= help "duration in seconds",
    height = Nothing &= typ "INT" &= help "initial height",
    width = Nothing &= typ "INT" &= help "initial width",
    x = Nothing &= typ "INT" &= help "initial x position",
    y = Nothing &= typ "INT" &= help "initial y position"}
    &= help "ffmpegcrop displays the first video frame with a gray rectangle overlay,\
        \ left click to move the closest corner to that point\
        \ and press Enter to transcode that region to a new file"

-- | output file path with the format string expanded
oup :: FFMpegCrop2 -> FilePath
oup FFMpegCrop2{..} = ep $ dropExtension input ++ uniq <.> format
 where ep = \case
        '%':'%':xs -> '%' : ep xs
        '%':'w':xs -> show width ++ ep xs
        '%':'h':xs -> show height ++ ep xs
        '%':'d':xs -> maybe "?" show duration ++ ep xs
        '%':'x':xs -> show x ++ ep xs
        '%':'y':xs -> show y ++ ep xs
        x:xs -> x : ep xs
        [] -> []

-- | the `crop=width:height:x:y` part of the ffmpeg command
cropstr :: (Int, Int) -> FFMpegCrop2 -> String
cropstr (w0, h0) FFMpegCrop2{..} = "crop=" ++ show width ++ ":" ++ show height ++ ":" ++ show x ++ ":" ++ show y

dim0 :: FilePath -> IO (Int, Int)
dim0 fp = do
  (_, wh) <- shellStrict ("ffprobe -v error -show_entries stream=width,height -of default=noprint_wrappers=1 " <> T.pack fp) empty
  let a:b:_ = mapMaybe (readMaybe . T.unpack . T.dropWhile (not . isDigit)) $ T.lines wh
  return (a,b)


toBMP :: Int -> FilePath -> IO Picture
toBMP n fp = do
  let cmd = "ffmpeg -y -i " <> fp <> " -vf \"select=" <> show n <> "\" -vframes 1 " <> out
      out = dropExtension fp <.> "bmp"
  shells (T.pack cmd) empty
  loadBMP out <* rm out


main = do
  c1 <- cmdArgs ffmpegCrop
  wh@(w,h) <- dim0 $ fromJust c1.input
  let c = c2 wh c1

  b <- toBMP 1 c.input

  crop <- newEmptyMVar

  forkIO $ interactIO (InWindow c.input wh (0,0)) white c
        ( \ c -> return $ renderCrop wh (c^.xywh) b)
        ( \ev c -> do
                let c' = c & xywh %~ clamp . mv
                      where clamp = clampXYWH (0,0,w,h)
                            mv = corners . vertices %~ updateCrop wh ev
                case ev of
                  EventKey key Down _ _
                        | key `elem` [SpecialKey KeyEnter] -> putMVar crop c'
                  _ -> return ()
                return c')
        ( \_ -> return ())
        
  forever $ do
    c <- takeMVar crop
    let cmd = "ffmpeg -y -i " <> c.input <> " -ss " <> show c.start
                <> maybe "" ((" -t " <>) . show) c.duration
                <> " -vf \"" <> cropstr wh c <> "\" "
                <> oup c <> " -hide_banner"
    shells (T.pack cmd) empty
    putStrLn ("finished writing:\n" <> oup c)
    exitSuccess

renderCrop :: (Int, Int) -> XYWH -> Picture -> Picture
renderCrop (w,h) xywh p = (p<>)
               $ Color (withAlpha 0.2 black)
               $ Polygon $ xywh ^.. corners . vertices . traversed 
                                . to (each %~ fromIntegral)
                                . to (\(x,y) -> (x - fromIntegral w/2, y - fromIntegral h/2))

type XYWH = (Int, Int, Int, Int)
type XYXY = (Int, Int, Int, Int)

xywh :: Lens' FFMpegCrop2 XYWH
xywh = lens g s
 where g FFMpegCrop2{..} = (x,y,width,height)
       s :: FFMpegCrop2 -> XYWH -> FFMpegCrop2
       s c (x,y,w,h) = c { x = x, y = y, width = w, height = h }

corners :: Iso' XYWH XYXY
corners = iso (\(x,y,w,h) -> (x,y,x+w,y+h)) (\(x,y,x',y') -> (min x' x,min y' y,abs $ x'-x,abs $ y'-y))

-- | `clampXYXY a b` is `b` the intersection of `a` and `b`
clampXYXY :: XYXY -> XYXY -> XYXY
clampXYXY (x,y,x',y') (x0,y0,x0',y0') = (clamp (x,x') x0, clamp (y,y') y0, clamp (x,x') x0', clamp (y,y') y0')

clampXYWH :: XYWH -> XYWH -> XYWH
clampXYWH a b = corners # clampXYXY (a^.corners) (b^.corners)

-- | doesn't check that the [(Int,Int)] is really a rectangle
vertices :: Lens' XYXY [(Int, Int)]
vertices f (x,y,x',y') = g <$> f [(x,y),(x',y),(x',y'),(x,y')]
 where g [(x_,y_),(x'_,y__),(x'__,y'_),(x__,y'__)] = (h x x_ x__,h y y_ y__,h x' x'_ x'__,h y' y'_ y'__)
       h o a b | o == a = b
               | otherwise = a

closestF :: Ord b => (a -> b) -> Traversal' [a] a
closestF ab f xs@(_:_) = ix i f xs
 where (i, _): _ = List.sortOn snd $ imap (\i x -> (i, ab x)) xs

-- | `closest q` accesses the first point closest to `q`, 
-- if you change it, it has to stay the closest point
closest, farthest :: (Int, Int) -> Traversal' [(Int, Int)] (Int, Int)
closest q f xs = closestF (d q) f xs
 where d (x,y) (x',y') = (x-x')^2 + (y-y')^2

farthest q = closestF (negate . d q)
 where d (x,y) (x',y') = (x-x')^2 + (y-y')^2

isRect :: [(Int, Int)] -> Bool
isRect xs@[_,_,_,_] = allOf each ((==2) . length . List.nub) (unzip xs)
isRect _ = False

-- currently when you click twice nothing happens because you're
-- moving a point to the old location.
-- Instead it could restore the previous wh, ie, instead of moving the
-- point it could move the opposite point.
-- I need to store the previous wh
updateCrop :: (Int, Int) -> Event -> [(Int,Int)] -> [(Int, Int)]
updateCrop (w,h) (EventKey (MouseButton LeftButton) Down _ (round -> a,round -> b)) pts
                = pts & closest xy .~ xy
                where xy = (a + div w 2,b + div h 2)
updateCrop _ _ x = x
