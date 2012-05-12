module Render (clearCanvas, CPosition(..), CSize(..), toDouble,
               renderWave, renderTurtle, renderPngFit, renderPngInline,
               renderLayoutG, renderLayoutM,
               yposSequence, renderSlide) where
import System.FilePath ((</>),(<.>))
import Control.Monad
import Control.Monad.Reader
import Text.Pandoc (Attr)
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
--
import FormatPangoMarkup
import Config
import WrapPaths (wrapGetDataFileName)

data CPosition = CPosition Double | CCenter
               deriving (Show, Eq, Ord)
data CSize = CSize Double | CFit
           deriving (Show, Eq, Ord)
type CXy = (CPosition, CPosition)
type CWl = (CSize, CSize)

toDouble :: Integral a => a -> Double
toDouble = fromIntegral

type LayoutFunc = G.PangoLayout -> G.Markup -> IO ()

renderLayout' :: String -> LayoutFunc -> CXy -> Double -> String -> C.Render Double
renderLayout' fname lFun (x, y) fsize text = do
  C.save
  (txt, lw, lh) <- liftIO $ do
    txt <- G.cairoCreateContext Nothing >>= G.layoutEmpty
    lFun txt text
    fd <- liftIO G.fontDescriptionNew
    G.fontDescriptionSetSize fd fsize
    G.fontDescriptionSetFamily fd fname
    G.layoutSetFontDescription txt (Just fd)
    (_, G.PangoRectangle _ _ lw lh) <- G.layoutGetExtents txt 
    -- xxx inkとlogicalの違いは？
    return (txt, lw, lh)
  let truePosition (CPosition x') (CPosition y') = return (x', y' + fsize)
      truePosition CCenter (CPosition y') =
        return (toDouble (canvasW gCfg) / 2 - lw / 2, y')
      truePosition x' y' =
        error $ "called with x=" ++ show x' ++ " y=" ++ show y'
  (xt, yt) <- truePosition x y
  C.moveTo xt yt
  G.showLayout txt
  C.restore
  return $ yt + lh

renderLayoutM :: CXy -> Double -> String -> C.Render Double
renderLayoutM = renderLayout' "IPA P明朝" G.layoutSetText

renderLayoutG :: Attr -> CXy -> Double -> String -> C.Render Double
renderLayoutG (_, [], _) xy fs txt = 
  renderLayout' "モトヤLマルベリ3等幅" G.layoutSetText xy fs txt
renderLayoutG (_, classs, _) xy fs txt =
  renderLayout' "モトヤLマルベリ3等幅" f xy fs txt'
    where
      txt' = formatPangoMarkup (head classs) txt
      f l t = void $ G.layoutSetMarkup l t

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
  ret surface w h
  where
    ret _ 0 0 = do 
      surface' <- liftIO $ 
                  wrapGetDataFileName ("data" </> "notfound" <.> "png") >>= 
                  C.imageSurfaceCreateFromPNG
      w' <- C.imageSurfaceGetWidth surface'
      h' <- C.imageSurfaceGetHeight surface'
      return (surface', w', h')
    ret s w h = return (s, w, h)

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

renderPngInline :: CXy -> CWl -> Double -> FilePath -> C.Render Double
renderPngInline = f
  where f (CCenter, CPosition y) (CFit, CFit) alpha file = do
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
        f _ _ _ _ = return 0 -- xxx renerPngFit統合して一関数にすべき

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
  _ <- renderLayoutM (CPosition 0, CPosition $ ch - ws * 2) ws $ replicate numChar '>'
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
