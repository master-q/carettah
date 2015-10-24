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

-- type LayoutFunc = G.PangoLayout -> G.Markup -> IO ()
type LayoutFunc = G.PangoLayout -> String -> IO ()
type LayoutFuncGlowing = String -> CXy -> Double -> String -> IO (G.PangoLayout, G.PangoLayout, Double, Double)

stringToLayout :: String -> LayoutFunc -> CXy -> Double -> String -> IO (G.PangoLayout, Double, Double)
stringToLayout fname func (x, _) fsize text = do
  lay <- G.cairoCreateContext Nothing >>= G.layoutEmpty
  void $ func lay text
  G.layoutSetWrap lay G.WrapPartialWords
  setAW lay x
  fd <- liftIO G.fontDescriptionNew
  G.fontDescriptionSetSize fd fsize
  G.fontDescriptionSetFamily fd fname
  G.layoutSetFontDescription lay (Just fd)
  (_, G.PangoRectangle _ _ lw lh) <- G.layoutGetExtents lay
  -- xxx inkとlogicalの違いは？
  return (lay, lw, lh)
    where
      screenW = toDouble (canvasW gCfg)
      setAW lay CCenter = do
        G.layoutSetWidth lay (Just screenW)
        G.layoutSetAlignment lay G.AlignCenter
      setAW lay (CPosition x') = do
        G.layoutSetWidth lay (Just $ screenW - x' * 2)
        G.layoutSetAlignment lay G.AlignLeft

truePosition :: Double -> Double -> (CPosition, CPosition) -> (Double, Double)
truePosition fsize _ (CPosition x', CPosition y') = (x', y' + fsize)
truePosition _ _ (CCenter, CPosition y') = (0, y')
truePosition _ _ (x', y') =
  error $ "called with x=" ++ show x' ++ " y=" ++ show y'

stringToLayoutGlowing :: LayoutFunc -> LayoutFunc -> LayoutFuncGlowing
stringToLayoutGlowing funcBack funcFront fname xy fsize text = do
  (layB, _, _) <- stringToLayout fname funcBack xy fsize text
  (lay, lw, lh) <- stringToLayout fname funcFront xy fsize text
  return (layB, lay, lw, lh)

renderLayout' :: String -> LayoutFuncGlowing -> CXy -> Double -> String -> C.Render Double
renderLayout' fname func (x, y) fsize text = do
  C.save
  (layB, lay, lw, lh) <- liftIO $ func fname (x, y) fsize text
  let (xt, yt) = truePosition fsize lw (x, y)
  mapM_ (moveShowLayout layB) 
    [(xt + xd, yt + yd) | xd <- [-0.7, 0.7], yd <- [-0.7, 0.7]]
  moveShowLayout lay (xt, yt)
  C.restore
  return $ yt + lh
  where
    moveShowLayout l (x', y') = C.moveTo x' y' >> G.showLayout l

renderLayoutM :: CXy -> Double -> String -> C.Render Double
renderLayoutM = 
  renderLayout' "IPA P明朝" (stringToLayoutGlowing fb ff)
  where
    fb l t = do _ <- G.layoutSetMarkup l ("<span foreground=\"white\">" ++ G.escapeMarkup t ++ "</span>") :: IO String
                return ()
    ff = G.layoutSetText

renderLayoutG' :: LayoutFuncGlowing -> CXy -> Double -> String -> C.Render Double
renderLayoutG' = renderLayout' "IPAゴシック"

renderLayoutG :: Attr -> CXy -> Double -> String -> C.Render Double
renderLayoutG (_, [], _) = 
  renderLayoutG' (stringToLayoutGlowing fb ff)
  where
    fb l t = do _ <- G.layoutSetMarkup l ("<span foreground=\"white\">" ++ G.escapeMarkup t ++ "</span>") :: IO String
                return ()
    ff = G.layoutSetText
renderLayoutG (_, classs, _) =
  renderLayoutG' (stringToLayoutGlowing fb ff)
  where
    fb l t = do _ <- G.layoutSetMarkup l (formatPangoMarkupWhite (head classs) t) :: IO String
                return ()
    ff l t = do _ <- G.layoutSetMarkup l (formatPangoMarkup (head classs) t) :: IO String
                return ()

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
  void $ renderLayoutM (CPosition 0, CPosition $ ch - ws * 2) ws $ replicate numChar '>'
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
  void $ yposSequence tcy s
  renderWave

renderSlide :: [[Double -> C.Render Double]] -> Int -> Int -> Int -> C.Render ()
renderSlide s p w h = do
  renderSlideFilter w h (s !! p)
  renderTurtle $ toDouble p / toDouble (length s - 1)
