import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C

import Data.Char
import Data.Bits
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Reader

import Control.Monad.Trans (liftIO)

type PresenSlide = [C.Render ()]

-- global value
data CarettahState = CarettahState { page :: Int }

carettahState :: IORef CarettahState
carettahState = unsafePerformIO $ newIORef CarettahState { page = 0 }

updateCarettahState :: MonadIO m => (CarettahState -> CarettahState) -> m ()
updateCarettahState fn = liftIO $! atomicModifyIORef carettahState $ \st -> (fn st, ())

queryCarettahState :: MonadIO m => (CarettahState -> a) -> m a
queryCarettahState fn = liftM fn $ liftIO $! readIORef carettahState

updatePage :: MonadIO m => (Int -> Int) -> m ()
updatePage fn = updateCarettahState (\s -> s { page = fn $ page s })

nextPage, prevPage :: MonadIO m => m ()
nextPage = let max = length myPresentation - 1
           in updatePage $ (\p -> if p >= max then max else p + 1)
prevPage = updatePage $ (\p -> if p == 0 then 0 else p - 1)

-- posX,posY,fsizeの値は640x480の画面サイズが基準
myPresentation :: [PresenSlide]
myPresentation = [
  -- 0
  [renderPngFit 0.3 "start_haskell.png",
   renderText 10 100 40 "C言語の世界と仲良しに!",
   renderText 100 200 20 "Kiwamu Okabe"],
  -- 1
  [renderText 10 100 40 "じゃじゃーんプレゼンツール作ったヨー"],
  -- 2
  [renderText 10 100 40 "内容どないっしょ"]
  ]

windowWidth, windowHeight :: Int
windowWidth   = 640
windowHeight  = 480

toDouble :: Int -> Double
toDouble = fromIntegral

screenSize :: Int -> Double -> Double
screenSize nowWidth size = size * (toDouble nowWidth / toDouble windowWidth)

-- copy from System.Glib.UTFString (gtk2hs/glib/System/Glib/UTFString.hs)
-- 本来はCStringを使うとこに埋め込んどくべき。gtk2hsを参考に
toUTF :: String -> String
toUTF [] = []
toUTF (x:xs) | ord x<=0x007F = x:toUTF xs
	     | ord x<=0x07FF = chr (0xC0 .|. ((ord x `shift` (-6)) .&. 0x1F)):
			       chr (0x80 .|. (ord x .&. 0x3F)):
			       toUTF xs
	     | otherwise     = chr (0xE0 .|. ((ord x `shift` (-12)) .&. 0x0F)):
			       chr (0x80 .|. ((ord x `shift` (-6)) .&. 0x3F)):
			       chr (0x80 .|. (ord x .&. 0x3F)):
			       toUTF xs

updateCanvas :: G.DrawingArea -> IO ()
updateCanvas canvas = do
  n <- queryCarettahState page
  win <- G.widgetGetDrawWindow canvas
  (width, height) <- G.widgetGetSize canvas
  G.renderWithDrawable win $
    renderSlide n width height

renderText :: Double -> Double -> Double -> String -> C.Render ()
renderText x y fsize text = do
  C.save
  C.selectFontFace (toUTF "Takao P明朝") C.FontSlantNormal C.FontWeightNormal
  C.setFontSize $ fsize
  C.moveTo x y
  C.textPath $ toUTF $ text
  C.fill
  C.stroke
  C.restore

renderSurface :: Double -> Double -> Double -> C.Surface -> C.Render ()
renderSurface x y alpha surface = do
  C.save
  C.setSourceSurface surface x y
  C.paintWithAlpha alpha
  C.restore

renderPngFit :: Double -> FilePath -> C.Render ()
renderPngFit alpha file = do
  C.save
  surface <- liftIO $ C.imageSurfaceCreateFromPNG file
  iw <- C.imageSurfaceGetWidth surface
  ih <- C.imageSurfaceGetHeight surface
  C.scale (toDouble windowWidth / toDouble iw) (toDouble windowHeight / toDouble ih)
  renderSurface 0 0 alpha surface
  C.restore

clearCanvas :: Int -> Int -> C.Render ()
clearCanvas w h = do
  C.save
  C.setSourceRGB 1 1 1
  C.rectangle 0 0 (toDouble w) (toDouble h)
  C.fill
  C.stroke
  C.restore

renderSlide :: Int -> Int -> Int -> C.Render ()
renderSlide s w h = do
  clearCanvas w h
  C.scale (toDouble w / toDouble windowWidth) (toDouble h / toDouble windowHeight)
  mapM_ id (myPresentation !! s)

main :: IO ()
main = do
  _ <- G.initGUI
  window <- G.windowNew
  canvas <- G.drawingAreaNew
  -- fix size
  --   G.windowSetResizable window False
  G.widgetSetSizeRequest window windowWidth windowHeight
  -- press key
  window `G.on` G.keyPressEvent $ G.tryEvent $ do
    keyName <- G.eventKeyName
    liftIO $
      case keyName of
        "f" -> G.windowFullscreen window
        "F" -> G.windowUnfullscreen window
        "q" -> G.widgetDestroy window
        "j" -> do nextPage
                  updateCanvas canvas
        "k" -> do prevPage
                  updateCanvas canvas
  _ <- G.onDestroy window G.mainQuit
  G.onExpose canvas $ const (do updateCanvas canvas
                                return True)
  G.set window [G.containerChild G.:= canvas]
  G.widgetShowAll window
  G.mainGUI
