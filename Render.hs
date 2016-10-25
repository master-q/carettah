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

stringToLayout :: Config -> String -> LayoutFunc -> CXy -> Double -> String -> IO (G.PangoLayout, Double, Double)
stringToLayout cfg fname func (x, _) fsize text = do
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
      screenW = toDouble (canvasW cfg)
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

stringToLayoutGlowing :: Config -> LayoutFunc -> LayoutFunc -> LayoutFuncGlowing
stringToLayoutGlowing cfg funcBack funcFront fname xy fsize text = do
  (layB, _, _) <- stringToLayout cfg fname funcBack xy fsize text
  (lay, lw, lh) <- stringToLayout cfg fname funcFront xy fsize text
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

renderLayoutM :: Config -> CXy -> Double -> String -> C.Render Double
renderLayoutM cfg =
  renderLayout' (fontNameP cfg) (stringToLayoutGlowing cfg fb ff)
  where
    fb l t = do _ <- G.layoutSetMarkup l ("<span foreground=\"white\">" ++ G.escapeMarkup t ++ "</span>") :: IO String
                return ()
    ff = G.layoutSetText

renderLayoutG' :: Config -> LayoutFuncGlowing -> CXy -> Double -> String -> C.Render Double
renderLayoutG' cfg = renderLayout' $ fontNameM cfg

renderLayoutG :: Config -> Attr -> CXy -> Double -> String -> C.Render Double
renderLayoutG cfg (_, [], _) = 
  renderLayoutG' cfg (stringToLayoutGlowing cfg fb ff)
  where
    fb l t = do _ <- G.layoutSetMarkup l ("<span foreground=\"white\">" ++ G.escapeMarkup t ++ "</span>") :: IO String
                return ()
    ff = G.layoutSetText
renderLayoutG cfg (_, classs, _) =
  renderLayoutG' cfg (stringToLayoutGlowing cfg fb ff)
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

renderPngInline :: Config -> CXy -> CWl -> Double -> FilePath -> C.Render Double
renderPngInline cfg = f
  where f (CCenter, CPosition y) (CFit, CFit) alpha file = do
          C.save
          (surface, iw, ih) <- pngSurfaceSize file
          let diw = toDouble iw
              dih = toDouble ih
              cw = toDouble (canvasW cfg)
              ch = toDouble (canvasH cfg)
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

renderPngFit :: Config -> Double -> FilePath -> C.Render ()
renderPngFit cfg = f
  where f alpha file = do
          C.save
          (surface, iw, ih) <- pngSurfaceSize file
          let cw = toDouble $ canvasW cfg
              ch = toDouble $ canvasH cfg
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
renderWave :: Config -> C.Render ()
renderWave cfg = do
  sec <- liftIO elapsedSecFromStart
  smin <- queryCarettahState speechMinutes
  let ws = waveSize cfg
      ch = toDouble $ canvasH cfg
      speechSec = 60 * smin
      charMax = waveCharMax cfg
      numChar = round $ charMax * sec / speechSec
  void $ renderLayoutM cfg (CPosition 0, CPosition $ ch - ws * 2) ws $ replicate numChar '>'
  return ()

renderTurtle :: Config -> Double -> C.Render ()
renderTurtle cfg progress = do
  fn <- liftIO . wrapGetDataFileName $ "data" </> "turtle" <.> "png"
  renderPngSize (ts / 2 + (cw - ts * 2) * progress) (ch - ts) ts ts 1 fn >> return ()
    where ts = turtleSize cfg
          cw = toDouble $ canvasW cfg
          ch = toDouble $ canvasH cfg

yposSequence :: Double -> [Double -> C.Render Double] -> C.Render Double
yposSequence ypos (x:xs) = x ypos >>= (`yposSequence` xs)
yposSequence ypos [] = return ypos

renderSlideFilter :: Config -> Int -> Int -> [Double -> C.Render Double] -> C.Render ()
renderSlideFilter cfg w h s = do
  clearCanvas w h
  let cw = toDouble $ canvasW cfg
      ch = toDouble $ canvasH cfg
      tcy = textContextY cfg
  C.scale (toDouble w / cw) (toDouble h / ch)
  void $ yposSequence tcy s
  renderWave cfg

renderSlide :: Config -> [[Double -> C.Render Double]] -> Int -> Int -> Int -> C.Render ()
renderSlide cfg s p w h = do
  renderSlideFilter cfg w h (s !! p)
  renderTurtle cfg $ toDouble p / toDouble (length s - 1)
