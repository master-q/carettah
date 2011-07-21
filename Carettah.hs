import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C

import System

import Data.Char
import Data.Bits
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Reader

import Control.Monad.Trans (liftIO)

import qualified Text.Pandoc as P
import qualified System.IO.UTF8 as U

markdown :: String -> P.Pandoc
markdown = P.readMarkdown P.defaultParserState{ P.stateStandalone = True }

type PresenSlide = [C.Render ()]

-- global value
data CarettahState = CarettahState { page :: Int, slides :: [PresenSlide] }

carettahState :: IORef CarettahState
carettahState = unsafePerformIO $ newIORef CarettahState { page = 0, slides = undefined }

updateCarettahState :: MonadIO m => (CarettahState -> CarettahState) -> m ()
updateCarettahState fn = liftIO $! atomicModifyIORef carettahState $ \st -> (fn st, ())

queryCarettahState :: MonadIO m => (CarettahState -> a) -> m a
queryCarettahState fn = liftM fn $ liftIO $! readIORef carettahState

updatePage :: MonadIO m => (Int -> Int) -> m ()
updatePage fn = updateCarettahState (\s -> s { page = fn $ page s })

nextPage, prevPage :: MonadIO m => m ()
nextPage = do s <- queryCarettahState slides
              let max = length s - 1
              updatePage (\p -> if p >= max then max else p + 1)
prevPage = updatePage (\p -> if p == 0 then 0 else p - 1)

updateSlides :: MonadIO m => ([PresenSlide] -> [PresenSlide]) -> m ()
updateSlides fn = updateCarettahState (\s -> s { slides = fn $ slides s })

-- posX,posY,fsizeの値は640x480の画面サイズが基準
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

mySetFontSize :: Double -> C.Render ()
mySetFontSize fsize = do
  C.selectFontFace (toUTF "Takao P明朝") C.FontSlantNormal C.FontWeightNormal
  C.setFontSize fsize

renderText :: Double -> Double -> Double -> String -> C.Render ()
renderText x y fsize text = do
  C.save
  mySetFontSize fsize
  C.moveTo x y
  C.textPath $ toUTF text
  C.fill
  C.stroke
  C.restore

renderTextCenter :: Double -> Double -> String -> C.Render ()
renderTextCenter y fsize text = do
  C.save
  mySetFontSize fsize
  (C.TextExtents xb yb w h _ _) <- C.textExtents (toUTF text)
  renderText (toDouble windowWidth / 2 - w / 2) y fsize text
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
renderSlide p w h = do
  s <- queryCarettahState slides
  clearCanvas w h
  C.scale (toDouble w / toDouble windowWidth) (toDouble h / toDouble windowHeight)
  mapM_ id (s !! p)

splitBlocks :: P.Pandoc -> [[P.Block]]
splitBlocks (P.Pandoc _ blocks) = go blocks
  where go (P.Header 1 h:xs) = let (b1, b2) = break (\a -> case a of
                                                      (P.Header 1 _) -> True
                                                      _ -> False) xs
                             in (P.Header 1 h:b1):go b2
        go _ = []

backgroundTop :: [P.Block] -> [P.Block]
backgroundTop blocks = filter go blocks ++ filter (not . go) blocks
  where go (P.Para [P.Image [P.Str "background"] _]) = True
        go _ = False

inlinesToString :: [P.Inline] -> String
inlinesToString = foldr go ""
  where go (P.Str s) a = s ++ a
        go P.Space a = " " ++ a
        go x _ = show x

blockToSlide :: [P.Block] -> PresenSlide
blockToSlide blockss = map go blockss
  where
    go (P.Para [P.Image [P.Str "background"] (pngfile, _)]) = 
      renderPngFit 0.3 pngfile
    go (P.Header 1 strs) =
      renderTextCenter 100 40 (inlinesToString strs)
    go (P.BulletList plains) = 
      renderTextCenter 300 30 (show plains) -- xxxx
    go (P.Para strs) = renderTextCenter 300 30 (inlinesToString strs)
    go x = error $ show x -- markdownで書いたもの一部のみをサポート

main :: IO ()
main = do
  -- parse markdown
  args <- getArgs
  s <- case args of
    (x:_) -> U.readFile x
    _     -> error "*** Need markdown filename."
  updateSlides $ const $ map (blockToSlide . backgroundTop) $ splitBlocks $ markdown s
  -- start GUI
  _ <- G.initGUI
  window <- G.windowNew
  canvas <- G.drawingAreaNew
  G.widgetSetSizeRequest window windowWidth windowHeight
  -- key event
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
        "r" -> print "reload" -- xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx
  _ <- G.onDestroy window G.mainQuit
  G.onExpose canvas $ const (do updateCanvas canvas
                                return True)
  G.set window [G.containerChild G.:= canvas]
  G.widgetShowAll window
  G.mainGUI
