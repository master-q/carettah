module Main where
import System.Environment
import System.Mem
import System.IO
import System.Console.GetOpt
import System.Exit
import Data.Time
import Data.Maybe
import Data.Version (showVersion)
import System.FilePath ((</>),(<.>))
import System.Directory (copyFile)
import Control.Monad
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import qualified Text.Pandoc as P
import System.CWiid
--
import Config
import Render
import WrapPaths

markdown :: String -> P.Pandoc
markdown s = r
  where Right r = P.readMarkdown P.def{ P.readerStandalone = True } $ s

splitBlocks :: P.Pandoc -> [[P.Block]]
splitBlocks (P.Pandoc _ blocks) = go blocks
  where go (P.Header 1 _ h:xs) =
          let (b1, b2) = break check xs
          in (P.Header 1 P.nullAttr h:b1):go b2
        go _ = []
        check (P.Header 1 _ _) = True
        check _ = False

backgroundTop :: [P.Block] -> [P.Block]
backgroundTop blocks = filter go blocks ++ filter (not . go) blocks
  where go (P.Para [P.Image _ [P.Str "background"] _]) = True
        go _ = False

inlinesToString :: [P.Inline] -> String
inlinesToString = foldr go ""
  where go (P.Str s) a = s ++ a
        go P.Space a = ' ' : a
        go x _ = show x

-- 二枚目以降のスライドをRender
blockToSlide :: [P.Block] -> [Double -> C.Render Double]
blockToSlide = map go
  where
    ag = alphaBackG gCfg
    tty = textTitleY gCfg
    tts = textTitleSize gCfg
    tcx = textContextX gCfg
    tcs = textContextSize gCfg
    tcbs = textCodeBlockSize gCfg
    tcbo = textCodeBlockOfs gCfg
    go :: P.Block -> Double -> C.Render Double
    go (P.Para [P.Image _ [P.Str "background"] (pngfile, _)]) =
      \y -> renderPngFit ag pngfile >> return y
    go (P.Para [P.Image _ [P.Str "inline"] (pngfile, _)]) =
      \y -> renderPngInline (CCenter, CPosition y) (CFit, CFit) 
            1 pngfile
    go (P.Header 1 _ strs) =
      \y -> renderLayoutM (CCenter, CPosition tty) tts (inlinesToString strs) >> return y
    go (P.BulletList plains) = \y -> yposSequence y $ map go' plains
      where
        go' [P.Plain strs] =
          \ypos -> renderLayoutM (CPosition tcx, CPosition ypos) tcs ("☆ " ++ inlinesToString strs)
        go' x = error $ show x -- 一部のみをサポート
    go (P.CodeBlock attr ss) = \y ->
      renderLayoutG attr (CPosition $ tcx + tcbo, CPosition y) tcbs ss
    go (P.Para strs) =
      \y -> renderLayoutM (CPosition tcx, CPosition y) tcs (inlinesToString strs)
    go x = error $ show x -- 一部のみをサポート

-- スライド表紙をRender
coverSlide :: [P.Block] -> [Double -> C.Render Double]
coverSlide = map go
  where
    ag = alphaBackG gCfg
    ttcy = textTitleCoverY gCfg
    ttcs = textTitleCoverSize gCfg
    tccy = textContextCoverY gCfg
    tccs = textContextCoverSize gCfg
    go :: P.Block -> Double -> C.Render Double
    go (P.Para [P.Image _ [P.Str "background"] (pngfile, _)]) =
      \y -> renderPngFit ag pngfile >> return y
    go (P.Header 1 _ strs) =
      \y -> renderLayoutM (CCenter, CPosition ttcy) ttcs (inlinesToString strs) >> return y
    go (P.Para strs) =
      \y -> renderLayoutM (CCenter, CPosition tccy) tccs (inlinesToString strs) >> return y
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
    "set presentation time with minutes"
  , Option "i"     ["info"]
    (NoArg (\ opts -> opts { optSlideInfo = True }))
    "show slide infomation"
  , Option "n"     ["new-slide"]
    (NoArg (\ opts -> opts { optNewTemp = True }))
    "create a new slide file and open it"
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
  void G.initGUI
  window <- G.windowNew
  canvas <- G.drawingAreaNew
  G.widgetSetSizeRequest window (canvasW gCfg) (canvasH gCfg)
  -- key event
  void $ window `G.on` G.keyPressEvent $ G.tryEvent $ do
    keyName <- G.eventKeyName
    liftIO $
      case T.unpack keyName of
        "f" -> G.windowFullscreen window
        "F" -> G.windowUnfullscreen window
        "q" -> G.widgetDestroy window
        "j" -> nextPage >> G.widgetQueueDraw canvas
        "k" -> prevPage >> G.widgetQueueDraw canvas
        "g" -> topPage >> G.widgetQueueDraw canvas
        "G" -> endPage >> G.widgetQueueDraw canvas
        "r" -> do md <- queryCarettahState markdownFname
                  loadMarkdown md
                  curPage >> G.widgetQueueDraw canvas
        _   -> return ()
  void $ G.onDestroy window G.mainQuit
  void $ G.onExpose canvas $ const (updateCanvas canvas >> return True)
  void $ G.timeoutAdd (do rtime <- queryCarettahState renderdTime
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
  -- getopts
  (opts, filen:_) <- carettahOpts =<< getArgs
  -- create file if -n option
  case opts of
    (Options {optNewTemp = True}) ->
      do tf <- wrapGetDataFileName $ "data" </> "turtle" <.> "png"
         copyFile tf ("turtle" <.> "png")
         df <- wrapGetDataFileName $ "data" </> "debian" <.> "png"
         copyFile df ("debian" <.> "png")
         writeFile filen ns
           where ns = "\
\# Presentation Title\n\
\![background](debian.png)\n\n\
\Your Name\n\n\
\# Slide Title\n\
\* item1\n\
\* item2\n\
\* item3\n\n\
\![inline](turtle.png)\n"
    _ -> return ()
  -- setup slide
  updateMarkdownFname $ const filen
  loadMarkdown filen
  -- start
  case opts of
    (Options {optSlideInfo = True}) ->
      do s <- queryCarettahState slides
         putStrLn $ "Page: " ++ show (length s)
    (Options {optPdfOutput = Just pdf}) ->
      outputPDF pdf
    (Options {optWiimote = wiiOn, optTime = Just presenTime}) ->
      startPresentation wiiOn presenTime
    _ -> error "NOTREACHED"
