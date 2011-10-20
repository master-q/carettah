module Main where
import System.Environment
import System.Mem
import System.IO
import System.Console.GetOpt
import System.Exit
import Data.Time
import Data.Maybe
import Data.Version (showVersion)
import Control.Monad.Reader
--import Control.Monad.State
--import Control.Monad.Trans
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import qualified Text.Pandoc as P
import System.CWiid
--
import Config
import Render
import WrapPaths

markdown :: String -> P.Pandoc
markdown = P.readMarkdown P.defaultParserState{ P.stateStandalone = True }

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
blockToSlide :: [P.Block] -> [Double -> C.Render Double]
blockToSlide blockss = map go blockss
  where
    ag = alphaBackG gCfg
    tty = textTitleY gCfg
    tts = textTitleSize gCfg
    tcx = textContextX gCfg
    tcs = textContextSize gCfg
    tcbs = textCodeBlockSize gCfg
    tcbo = textCodeBlockOfs gCfg
    go :: P.Block -> Double -> C.Render Double
    go (P.Para [P.Image [P.Str "background"] (pngfile, _)]) =
      \y -> renderPngFit ag pngfile >> return y
    go (P.Para [P.Image [P.Str "inline"] (pngfile, _)]) =
      \y -> renderPngInline CairoCenter (CairoPosition y) CairoFit
            CairoFit 1 pngfile
    go (P.Header 1 strs) =
      \y -> renderText CairoCenter (CairoPosition tty) tts (inlinesToString strs) >> return y
    go (P.BulletList plains) = \y -> yposSequence y $ map go' plains
      where
        go' [P.Plain strs] =
          \ypos -> renderText (CairoPosition tcx) (CairoPosition ypos) tcs ("☆ " ++ inlinesToString strs)
        go' x = error $ show x -- 一部のみをサポート
    go (P.CodeBlock (_, _, _) ss) = \y -> yposSequence y $ map go' (lines ss)
      where
        go' s ypos = renderText (CairoPosition $ tcx + tcbo) (CairoPosition ypos) tcbs s
    go (P.Para strs) =
      \y -> renderText (CairoPosition tcx) (CairoPosition y) tcs (inlinesToString strs)
    go x = error $ show x -- 一部のみをサポート

-- スライド表紙をRender
coverSlide :: [P.Block] -> [Double -> C.Render Double]
coverSlide blocks = map go blocks
  where
    ag = alphaBackG gCfg
    ttcy = textTitleCoverY gCfg
    ttcs = textTitleCoverSize gCfg
    tccy = textContextCoverY gCfg
    tccs = textContextCoverSize gCfg
    go :: P.Block -> Double -> C.Render Double
    go (P.Para [P.Image [P.Str "background"] (pngfile, _)]) =
      \y -> renderPngFit ag pngfile >> return y
    go (P.Header 1 strs) =
      \y -> renderText CairoCenter (CairoPosition ttcy) ttcs (inlinesToString strs) >> return y
    go (P.Para strs) =
      \y -> renderText CairoCenter (CairoPosition tccy) tccs (inlinesToString strs) >> return y
    go x = error $ show x -- 一部のみをサポート

updateCanvas :: G.DrawingArea -> IO ()
updateCanvas canvas = do
  n <- queryCarettahState page
  s <- queryCarettahState slides
  win <- G.widgetGetDrawWindow canvas
  (width, height) <- G.widgetGetSize canvas
  G.renderWithDrawable win $
    renderSlide s n width height
  updateRenderdTime
  performGC

options :: [OptDescr (Options -> Options)]
options =
  [ Option "w"     ["wiimote"]
    (NoArg (\ opts -> opts { optWiimote = True }))
    "use wiimote"
  , Option "o"     ["output-filename"]
    (OptArg ((\ f opts -> opts { optPdfOutput = Just f }) . fromMaybe "output.pdf")
     "FILE")
    "output PDF_FILE"
  , Option "t"     ["time"]
    (OptArg ((\ f opts -> opts { optTime = Just $ read f }) . fromMaybe "5")
     "TIME(minute)")
    "set presentation time with minute"
  , Option "c"     ["count-page"]
    (NoArg (\ opts -> opts { optCountPage = True }))
    "count total page of slides"
  ]

carettahOpts :: [String] -> IO (Options, [String])
carettahOpts argv =
  let header = "\ncarettah version " ++ showVersion wrapVersion ++ "\n" ++
               "Usage: carettah [OPTION...] FILE"
  in case getOpt Permute options argv of
    (_,[],[] ) -> hPutStrLn stderr (usageInfo header options) >> exitSuccess
    (o,n,[]  ) -> return (foldl (flip id) defaultOptions o, n)
    (_,_,errs) -> hPutStrLn stderr (concat errs ++ usageInfo header options) >> exitFailure

outputPDF :: String -> IO ()
outputPDF pdf = do
  s <- queryCarettahState slides
  let iw = canvasW gCfg
      ih = canvasH gCfg
      dw = toDouble iw
      dh = toDouble ih
  C.withPDFSurface pdf dw dh $ flip C.renderWith . sequence_ $
    fmap (\a -> renderSlide s a iw ih >> C.showPage) [0..(length s - 1)]

startPresentation :: Bool -> Double -> IO ()
startPresentation wiiOn presenTime = do
  -- setup
  setWiiHandle wiiOn
  updateSpeechMinutes $ const presenTime
  -- start GUI
  _ <- G.initGUI
  window <- G.windowNew
  canvas <- G.drawingAreaNew
  G.widgetSetSizeRequest window (canvasW gCfg) (canvasH gCfg)
  -- key event
  _ <- window `G.on` G.keyPressEvent $ G.tryEvent $ do
    keyName <- G.eventKeyName
    liftIO $
      case keyName of
        "f" -> G.windowFullscreen window
        "F" -> G.windowUnfullscreen window
        "q" -> G.widgetDestroy window
        "j" -> nextPage >> G.widgetQueueDraw canvas
        "k" -> prevPage >> G.widgetQueueDraw canvas
        "r" -> do md <- queryCarettahState markdownFname
                  loadMarkdown md
                  curPage >> G.widgetQueueDraw canvas
        _   -> return ()
  _ <- G.onDestroy window G.mainQuit
  _ <- G.onExpose canvas $ const (updateCanvas canvas >> return True)
  _ <- G.timeoutAdd (do rtime <- queryCarettahState renderdTime
                        ntime <- getCurrentTime
                        let dtime :: Double
                            dtime = (fromRational . toRational) $
                                    diffUTCTime ntime rtime
                        if dtime > 5 then G.widgetQueueDraw canvas >>
                                          return True else do
                          bf <- queryCarettahState wiiBtnFlag
                          af <- updateWiiBtnFlag
                          let bs = af `diffCwiidBtnFlag` bf
                              go b | b == cwiidBtnA = nextPage >> G.widgetQueueDraw canvas
                                   | b == cwiidBtnB = prevPage >> G.widgetQueueDraw canvas
                                   | b == cwiidBtnUp = topPage >> G.widgetQueueDraw canvas
                                   | b == cwiidBtnDown = endPage >> G.widgetQueueDraw canvas
                                   | b == cwiidBtnPlus = G.windowFullscreen window
                                   | b == cwiidBtnMinus = G.windowUnfullscreen window
                                   | otherwise = return ()
                          go bs
                          return True) 50
  G.set window [G.containerChild G.:= canvas]
  G.widgetShowAll window
  updateStartTime
  updateRenderdTime
  G.mainGUI

loadMarkdown :: String -> IO ()
loadMarkdown fn = do
  s <- readFile fn
  let z = zip (coverSlide:repeat blockToSlide) (splitBlocks $ markdown s)
  updateSlides $ const $ map (\p -> fst p . backgroundTop $ snd p) z

main :: IO ()
main = do
  -- init
  updateStartTime
  updateRenderdTime
  -- opts
  (Options {optWiimote = wiiOn, optPdfOutput = pdfFilen,
            optTime = Just presenTime, optCountPage = countOn}, filen:_) <-
    carettahOpts =<< getArgs
  updateMarkdownFname $ const filen
  loadMarkdown filen
  -- count page
  if countOn then do s <- queryCarettahState slides
                     print $ length s
    else case pdfFilen of Just pdf -> outputPDF pdf
                          Nothing  -> startPresentation wiiOn presenTime
