module Render (clearCanvas, CairoPosition(..), CairoSize(..), toDouble,
               renderWave, renderTurtle, renderPngFit, renderPngInline,
               renderLayoutG, renderLayoutM,
               yposSequence, renderSlide) where
import System.FilePath ((</>),(<.>))
import Control.Monad.Reader
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
--
import Config
import WrapPaths (wrapGetDataFileName)

data CairoPosition = CairoPosition Double | CairoCenter
                   deriving (Show, Eq, Ord)
data CairoSize = CairoSize Double | CairoFit
                   deriving (Show, Eq, Ord)

toDouble :: Integral a => a -> Double
toDouble = fromIntegral

renderLayout' :: String -> CairoPosition -> CairoPosition -> Double -> String -> C.Render Double
renderLayout' fname x y fsize text = do
  C.save
  ctxt <- liftIO $ G.cairoCreateContext Nothing
  txt <- liftIO $ G.layoutEmpty ctxt
  liftIO $ G.layoutSetText txt text
  fd <- liftIO G.fontDescriptionNew
  liftIO $ G.fontDescriptionSetSize fd fsize
  liftIO $ G.fontDescriptionSetFamily fd fname
  liftIO $ G.layoutSetFontDescription txt (Just fd)
  (_, G.PangoRectangle _ _ lw lh) <-
    liftIO $ G.layoutGetExtents txt -- xxx inkとlogicalの違いは？
  let truePosition (CairoPosition x') (CairoPosition y') = return (x', y' + fsize)
      truePosition CairoCenter (CairoPosition y') =
        return (toDouble (canvasW gCfg) / 2 - lw / 2, y')
      truePosition x' y' =
        error $ "called with x=" ++ show x' ++ " y=" ++ show y'
  (xt, yt) <- truePosition x y
  C.moveTo xt yt
  G.showLayout txt
  C.restore
  return $ yt + lh

renderLayoutG, renderLayoutM :: CairoPosition -> CairoPosition -> Double -> String -> C.Render Double
renderLayoutG = renderLayout' "モトヤLマルベリ3等幅"
renderLayoutM = renderLayout' "IPA P明朝"

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
  if (w, h) == (0, 0)
    then do fn <- liftIO . wrapGetDataFileName $ "data" </> "notfound" <.> "png"
            surface' <- liftIO $ C.imageSurfaceCreateFromPNG fn
            w' <- C.imageSurfaceGetWidth surface'
            h' <- C.imageSurfaceGetHeight surface'
            return (surface', w', h')
    else return (surface, w, h)

renderPngSize :: Double -> Double -> Double -> Double -> Double -> FilePath -> C.Render Double
renderPngSize = f
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
renderPngInline = f
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
renderPngFit = f
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
  _ <- renderLayoutM (CairoPosition 0) (CairoPosition $ ch - ws * 2) ws $ replicate numChar '>'
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
