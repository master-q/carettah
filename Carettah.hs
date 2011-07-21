module Main where
import System
import Data.Char
import Data.Bits
import Data.IORef
import System.IO.Unsafe (unsafePerformIO)
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Trans
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import qualified Text.Pandoc as P

markdown :: String -> P.Pandoc
markdown = P.readMarkdown P.defaultParserState{ P.stateStandalone = True }

-- global value
data CarettahState = CarettahState { page :: Int, slides :: [[C.Render ()]] }

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

updateSlides :: MonadIO m => ([[C.Render ()]] -> [[C.Render ()]]) -> m ()
updateSlides fn = updateCarettahState (\s -> s { slides = fn $ slides s })

-- constant value
--- posX,posY,fsizeの値は640x480の画面サイズが基準
windowWidth, windowHeight :: Int
windowWidth   = 640
windowHeight  = 480
pngFitAlpha,textTitleY, textTitleSize, textContextY, textContextSize, textTitleCoverY, textTitleCoverSize, textContextX, textContextCoverY, textContextCoverSize :: Double
pngFitAlpha = 0.3
textTitleCoverY = 220
textTitleCoverSize = 40
textContextCoverY = 350
textContextCoverSize = 30
textTitleY = 100
textTitleSize = 40
textContextX = 40
textContextY = 150
textContextSize = 30

toDouble :: Int -> Double
toDouble = fromIntegral

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

renderTextNextline :: Double -> Double -> String -> Double -> C.Render Double
renderTextNextline x fsize text ypos = do
  C.save
  mySetFontSize fsize
  (C.TextExtents xb yb w h _ _) <- C.textExtents (toUTF text)
  C.restore
  let nypos = ypos + (h * 1.4)
  renderText x nypos fsize text
  return nypos

renderTextCenter :: Double -> Double -> String -> C.Render ()
renderTextCenter y fsize text = do
  C.save
  mySetFontSize fsize
  (C.TextExtents xb yb w h _ _) <- C.textExtents (toUTF text)
  C.restore
  renderText (toDouble windowWidth / 2 - w / 2) y fsize text

renderSurface :: Double -> Double -> Double -> C.Surface -> C.Render ()
renderSurface x y alpha surface = do
  C.save
  C.setSourceSurface surface x y
  C.paintWithAlpha alpha
  C.restore

pngSurfaceSize :: FilePath -> C.Render (C.Surface, Int, Int)
pngSurfaceSize file = do
  surface <- liftIO $ C.imageSurfaceCreateFromPNG file
  w <- C.imageSurfaceGetWidth surface
  h <- C.imageSurfaceGetHeight surface
  return (surface, w, h)

renderPngSize :: Double -> Double -> Double -> Double -> Double -> FilePath -> C.Render ()
renderPngSize x y w h alpha file = do
  C.save
  (surface, iw, ih) <- pngSurfaceSize file
  let xscale = w / toDouble iw
  let yscale = h / toDouble ih
  C.scale xscale yscale
  renderSurface (x / xscale) (y / yscale) alpha surface
  C.restore

renderPngFit :: Double -> FilePath -> C.Render ()
renderPngFit alpha file = do
  C.save
  (surface, iw, ih) <- pngSurfaceSize file
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

renderTurtle :: Double -> C.Render ()
renderTurtle progress =
  renderPngSize (20 + (640 - 40 - 40) * progress) 430 40 40 1 "turtle.png"

renderSlide :: Int -> Int -> Int -> C.Render ()
renderSlide p w h = do
  s <- queryCarettahState slides
  clearCanvas w h
  C.scale (toDouble w / toDouble windowWidth) (toDouble h / toDouble windowHeight)
  sequence_ (s !! p)
  renderTurtle $ toDouble p / toDouble (length s - 1)

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
        go P.Space a = ' ' : a
        go x _ = show x

-- 二枚目以降のスライドをRender
blockToSlide :: [P.Block] -> [C.Render ()]
blockToSlide blockss = map go blockss
  where
    go (P.Para [P.Image [P.Str "background"] (pngfile, _)]) =
      renderPngFit pngFitAlpha pngfile
    go (P.Header 1 strs) =
      renderTextCenter textTitleY textTitleSize (inlinesToString strs)
    go (P.BulletList plains) = go'' textContextY $ map go' plains
      where
        go'' ypos [] = return ()
        go'' ypos (x:xs) = x ypos >>= (`go''` xs)
        go' [P.Plain strs] = renderTextNextline textContextX textContextSize ("☆ " ++ inlinesToString strs)
        go' x = error $ show x -- 一部のみをサポート
    go (P.Para strs) = do renderTextNextline textContextX textContextSize (inlinesToString strs) textContextY
                          return ()
    go x = error $ show x -- 一部のみをサポート

-- スライド表紙をRender
coverSlide :: [P.Block] -> [C.Render ()]
coverSlide blocks = map go blocks
  where
    go (P.Para [P.Image [P.Str "background"] (pngfile, _)]) =
      renderPngFit pngFitAlpha pngfile
    go (P.Header 1 strs) =
      renderTextCenter textTitleCoverY textTitleCoverSize (inlinesToString strs)
    go (P.Para strs) = renderTextCenter textContextCoverY textContextCoverSize (inlinesToString strs)
    go x = error $ show x -- 一部のみをサポート

updateCanvas :: G.DrawingArea -> IO ()
updateCanvas canvas = do
  n <- queryCarettahState page
  win <- G.widgetGetDrawWindow canvas
  (width, height) <- G.widgetGetSize canvas
  G.renderWithDrawable win $
    renderSlide n width height

main :: IO ()
main = do
  -- parse markdown
  args <- getArgs
  s <- case args of
    (x:_) -> readFile x
    _     -> error "*** Need markdown filename."
  let z = zip (coverSlide:repeat blockToSlide) (splitBlocks $ markdown s)
    in updateSlides $ const $ map (\p -> fst p . backgroundTop $ snd p) z
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
