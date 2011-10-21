module Render (clearCanvas, CairoPosition(..), CairoSize(..), toDouble,
               renderWave, renderTurtle, renderPngFit, renderPngInline,
               renderTextM, renderTextG, yposSequence, renderSlide) where
import Data.Char
import Data.Bits
import qualified Data.MemoUgly as M
import System.FilePath ((</>),(<.>))
import Control.Monad.Reader
import qualified Graphics.Rendering.Cairo as C
--
import Config
import WrapPaths (wrapGetDataFileName)

data CairoPosition = CairoPosition Double | CairoCenter
                   deriving (Show, Eq, Ord)
data CairoSize = CairoSize Double | CairoFit
                   deriving (Show, Eq, Ord)

-- memoize
memo2 :: (Ord a, Ord b) => (a -> b -> v) -> a -> b -> v
memo2 v = M.memo (M.memo . v)
memo3 :: (Ord a, Ord b, Ord c) => (a -> b -> c -> v) -> a -> b -> c -> v
memo3 v = M.memo (memo2 . v)
memo4 :: (Ord a, Ord b, Ord c, Ord d) =>
         (a -> b -> c -> d -> v) -> a -> b -> c -> d -> v
memo4 v = M.memo (memo3 . v)
memo5 :: (Ord a, Ord b, Ord c, Ord d, Ord e) =>
         (a -> b -> c -> d -> e -> v) -> a -> b -> c -> d -> e -> v
memo5 v = M.memo (memo4 . v)
memo6 :: (Ord a, Ord b, Ord c, Ord d, Ord e, Ord f) =>
         (a -> b -> c -> d -> e -> f -> v) -> a -> b -> c -> d -> e -> f -> v
memo6 v = M.memo (memo5 . v)

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

toDouble :: Integral a => a -> Double
toDouble = fromIntegral

mySetFontSize :: String -> Double -> C.Render ()
mySetFontSize fname fsize = do
  C.selectFontFace (toUTF fname) C.FontSlantNormal C.FontWeightNormal
  C.setFontSize fsize

renderText' :: String -> CairoPosition -> CairoPosition -> Double -> String -> C.Render Double
renderText' fname x y fsize text = do
  C.save
  mySetFontSize fname fsize
  (C.TextExtents _ yb w h _ _) <- C.textExtents (toUTF text)
  C.restore
  let truePosition (CairoPosition x') (CairoPosition y') = return (x', y')
      truePosition CairoCenter (CairoPosition y') =
          return (toDouble (canvasW gCfg) / 2 - w / 2, y')
      truePosition x' y' =
        error $ "called with x=" ++ show x' ++ " y=" ++ show y'
  (xt, yt) <- truePosition x y
  let nypos = yt + h - yb
  C.save
  mySetFontSize fname fsize
  C.moveTo xt nypos
  C.textPath $ toUTF text
  C.fill >> C.stroke >> C.restore
  return nypos

renderTextG :: CairoPosition -> CairoPosition -> Double -> String -> C.Render Double
renderTextG = renderText' "TakaoExゴシック"
renderTextM :: CairoPosition -> CairoPosition -> Double -> String -> C.Render Double
renderTextM = renderText' "Takao P明朝"

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

renderPngSize :: Double -> Double -> Double -> Double -> Double -> FilePath -> C.Render Double
--renderPngSize x y w h alpha file = memo6 f
renderPngSize = memo6 f
  where f x y w h alpha file = do
          C.save
          (surface, iw, ih) <- pngSurfaceSize file
          let xscale = w / toDouble iw
          let yscale = h / toDouble ih
          C.scale xscale yscale
          renderSurface (x / xscale) (y / yscale) alpha surface
          C.surfaceFinish surface
          C.restore
          return $ y + h

renderPngInline :: CairoPosition -> CairoPosition -> CairoSize -> CairoSize -> Double -> FilePath -> C.Render Double
renderPngInline = memo6 f
  where f CairoCenter (CairoPosition y) CairoFit CairoFit alpha file = do
          C.save
          (surface, iw, ih) <- pngSurfaceSize file
          let diw = toDouble iw
              dih = toDouble ih
              cw = toDouble (canvasW gCfg)
              ch = toDouble (canvasH gCfg)
              wratio = cw / diw
              hratio = (ch - y) / dih
              scale = if wratio > hratio then hratio * 0.95 else wratio * 0.95
              tiw = diw * scale
              tih = dih * scale
              y' = y + 10
          C.scale scale scale
          renderSurface ((cw / 2 - tiw / 2) / scale) (y' / scale) alpha surface
          C.surfaceFinish surface
          C.restore
          return $ y' + tih
        f _ _ _ _ _ _ = return 0 -- xxx renerPngFit統合して一関数にすべき

renderPngFit :: Double -> FilePath -> C.Render ()
renderPngFit = memo2 f
  where f alpha file = do
          C.save
          (surface, iw, ih) <- pngSurfaceSize file
          let cw = toDouble $ canvasW gCfg
              ch = toDouble $ canvasH gCfg
          C.scale (cw / toDouble iw) (ch / toDouble ih)
          renderSurface 0 0 alpha surface
          C.surfaceFinish surface
          C.restore

clearCanvas :: Int -> Int -> C.Render ()
clearCanvas w h = do
  C.save
  C.setSourceRGB 1 1 1
  C.rectangle 0 0 (toDouble w) (toDouble h)
  C.fill >> C.stroke >> C.restore

-- xxx プレゼン時間に応じて波表示
renderWave :: C.Render ()
renderWave = do
  sec <- liftIO elapsedSecFromStart
  smin <- queryCarettahState speechMinutes
  let ws = waveSize gCfg
      ch = toDouble $ canvasH gCfg
      speechSec = 60 * smin
      charMax = waveCharMax gCfg
      numChar = round $ charMax * sec / speechSec
  _ <- renderTextM (CairoPosition 0) (CairoPosition $ ch - ws) ws $ replicate numChar '>'
  return ()

renderTurtle :: Double -> C.Render ()
renderTurtle progress = do
  fn <- liftIO . wrapGetDataFileName $ "data" </> "turtle" <.> "png"
  renderPngSize (ts / 2 + (cw - ts * 2) * progress) (ch - ts) ts ts 1 fn >> return ()
    where ts = turtleSize gCfg
          cw = toDouble $ canvasW gCfg
          ch = toDouble $ canvasH gCfg

yposSequence :: Double -> [Double -> C.Render Double] -> C.Render Double
yposSequence ypos (x:xs) = x ypos >>= (`yposSequence` xs)
yposSequence ypos [] = return ypos

renderSlideFilter :: Int -> Int -> [Double -> C.Render Double] -> C.Render ()
renderSlideFilter w h s = do
  clearCanvas w h
  let cw = toDouble $ canvasW gCfg
      ch = toDouble $ canvasH gCfg
      tcy = textContextY gCfg
  C.scale (toDouble w / cw) (toDouble h / ch)
  _ <- yposSequence tcy s
  renderWave

renderSlide :: [[Double -> C.Render Double]] -> Int -> Int -> Int -> C.Render ()
renderSlide s p w h = do
  renderSlideFilter w h (s !! p)
  renderTurtle $ toDouble p / toDouble (length s - 1)
