module Config (Config(..), Options(..), CarettahState(..),
               gCfg, defaultOptions,
               curPage, nextPage, prevPage, topPage, endPage,
               setWiiHandle, updateWiiBtnFlag,
               updateSlides, queryCarettahState,
               updateStartTime, updateRenderdTime, elapsedSecFromStart,
               updateSpeechMinutes, updateMarkdownFname) where

import Data.IORef
import Data.Time
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad
import Control.Monad.Reader
import qualified Graphics.Rendering.Cairo as C
import System.CWiid

data Options = Options { optWiimote   :: Bool
                       , optPdfOutput :: Maybe FilePath
                       , optTime      :: Maybe Double
                       , optSlideInfo :: Bool
                       , optNewTemp   :: Bool
                       } deriving Show

defaultOptions :: Options
defaultOptions = Options { optWiimote   = False
                         , optPdfOutput = Nothing
                         , optTime      = Just 5
                         , optSlideInfo = False
                         , optNewTemp   = False
                         }

data WiiHandle = NoWiiHandle | WiiHandle CWiidWiimote
data CarettahState = CarettahState {
  page :: Int,
  slides :: [[Double -> C.Render Double]],
  startTime :: UTCTime,
  renderdTime :: UTCTime,
  wiiHandle :: WiiHandle,
  wiiBtnFlag :: CWiidBtnFlag,
  speechMinutes :: Double,
  markdownFname :: String
  }

carettahState :: IORef CarettahState
carettahState = unsafePerformIO $ newIORef CarettahState { page = 0, slides = undefined, startTime = undefined, renderdTime = undefined, wiiHandle = NoWiiHandle, wiiBtnFlag = CWiidBtnFlag 0 , speechMinutes = 5, markdownFname = "notfound.md"}

updateCarettahState :: MonadIO m => (CarettahState -> CarettahState) -> m ()
updateCarettahState fn = liftIO $! atomicModifyIORef carettahState $ \st -> (fn st, ())

queryCarettahState :: MonadIO m => (CarettahState -> a) -> m a
queryCarettahState fn = liftM fn $ liftIO $! readIORef carettahState

updatePage :: MonadIO m => (Int -> Int) -> m ()
updatePage fn = updateCarettahState (\s -> s { page = fn $ page s })

curPage, nextPage, prevPage, topPage, endPage :: MonadIO m => m ()
curPage = do s <- queryCarettahState slides
             let maxpage = length s - 1
             updatePage (\p -> if p >= maxpage then maxpage else p)
nextPage = do s <- queryCarettahState slides
              let maxpage = length s - 1
              updatePage (\p -> if p >= maxpage then maxpage else p + 1)
prevPage = updatePage (\p -> if p == 0 then 0 else p - 1)
topPage = updatePage $ const 0
endPage = do s <- queryCarettahState slides
             updatePage $ const (length s - 1)

updateSlides :: MonadIO m => ([[Double -> C.Render Double]] -> [[Double -> C.Render Double]]) -> m ()
updateSlides fn = updateCarettahState (\s -> s { slides = fn $ slides s })

updateStartTime :: IO ()
updateStartTime = do
  t <- getCurrentTime
  updateCarettahState (\s -> s { startTime = t })

updateRenderdTime :: IO ()
updateRenderdTime = do
  t <- getCurrentTime
  updateCarettahState (\s -> s { renderdTime = t })

elapsedSecFromStart :: IO Double
elapsedSecFromStart = do
  n <- getCurrentTime
  s <- queryCarettahState startTime
  let d = diffUTCTime n s
  return $ (fromRational . toRational) d

setWiiHandle :: Bool -> IO ()
setWiiHandle won
  | won = do
    putStrLn "Put Wiimote in discoverable mode now (press 1+2)..."
    wm <- cwiidOpen
    case wm of
      Nothing  -> putStrLn "not found..."
      Just wmj -> do
        putStrLn "found!"
        void $ cwiidSetRptMode wmj 2
        void $ cwiidSetLed wmj (combineCwiidLedFlag [cwiidLed1, cwiidLed4])
        updateCarettahState (\s -> s { wiiHandle = WiiHandle wmj })
  | otherwise = return ()

updateWiiBtnFlag :: IO CWiidBtnFlag
updateWiiBtnFlag = do
  wh <- queryCarettahState wiiHandle
  let go NoWiiHandle = return $ CWiidBtnFlag 0
      go (WiiHandle wm) = do
        bs <- cwiidGetBtnState wm
        updateCarettahState (\s -> s { wiiBtnFlag = bs })
        return bs
  go wh

updateSpeechMinutes :: MonadIO m => (Double -> Double) -> m ()
updateSpeechMinutes fn =
  updateCarettahState (\s -> s { speechMinutes = fn $ speechMinutes s })

updateMarkdownFname :: MonadIO m => (String -> String) -> m ()
updateMarkdownFname fn =
  updateCarettahState (\s -> s { markdownFname = fn $ markdownFname s })

-- constant value
data Config = Config {
  --- posX,posY,fsizeの値は640x480の画面サイズが基準
  canvasW :: Int,
  canvasH :: Int,
  alphaBackG :: Double,
  textTitleY :: Double,
  textTitleSize :: Double,
  textContextY :: Double,
  textContextSize :: Double,
  textTitleCoverY :: Double,
  textTitleCoverSize :: Double,
  textContextX :: Double,
  textContextCoverY :: Double,
  textContextCoverSize :: Double,
  textCodeBlockSize :: Double,
  textCodeBlockOfs :: Double,
  turtleSize :: Double,
  waveSize :: Double,
  waveCharMax :: Double
  }
gCfg :: Config
gCfg = Config {
  canvasW   = 640,
  canvasH  = 480,
  alphaBackG = 0.3,
  textTitleCoverY = 170,
  textTitleCoverSize = 28,
  textContextCoverY = 300,
  textContextCoverSize = 26,
  textTitleY = 35,
  textTitleSize = 26,
  textContextX = 40,
  textContextY = 90,
  textContextSize = 18,
  textCodeBlockSize = 11,
  textCodeBlockOfs = 10,
  turtleSize = 40,
  waveSize = 20,
  waveCharMax = 42 -- xxxxxx 本来はwaveSizeから検出すべき手で数えんなよwwww
  }
