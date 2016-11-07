module Main where
import System.Environment
import System.Mem
import System.IO
import System.Console.GetOpt
import System.Exit
import Data.Time
import Data.Maybe
import Data.Either
import Data.Version (showVersion)
import System.FilePath ((</>),(<.>))
import System.Directory (copyFile)
import Control.Monad
import Control.Monad.Reader
import qualified Data.Text as T
import qualified Graphics.UI.Gtk as G
import qualified Graphics.Rendering.Cairo as C
import qualified Text.Pandoc as P
import qualified Config as CFG
import System.CWiid
--
import Carettah.Config
import Carettah.Render
import Carettah.WrapPaths

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
blockToSlide :: Config -> [P.Block] -> [Double -> C.Render Double]
blockToSlide cfg = map go
  where
    ag = alphaBackG cfg
    tty = textTitleY cfg
    tts = textTitleSize cfg
    tcx = textContextX cfg
    tcs = textContextSize cfg
    tcbs = textCodeBlockSize cfg
    tcbo = textCodeBlockOfs cfg
    go :: P.Block -> Double -> C.Render Double
    go (P.Para [P.Image _ [P.Str "background"] (pngfile, _)]) =
      \y -> renderPngFit cfg ag pngfile >> return y
    go (P.Para [P.Image _ [P.Str "inline"] (pngfile, _)]) =
      \y -> renderPngInline cfg (CCenter, CPosition y) (CFit, CFit) 1 pngfile
    go (P.Header 1 _ strs) =
      \y -> renderLayoutM cfg (CCenter, CPosition tty) tts (inlinesToString strs) >> return y
    go (P.BulletList plains) = \y -> yposSequence y $ map go' plains
      where
        go' [P.Plain strs] =
          \ypos -> renderLayoutM cfg (CPosition tcx, CPosition ypos) tcs ("☆ " ++ inlinesToString strs)
        go' x = error $ show x -- 一部のみをサポート
    go (P.CodeBlock attr ss) = \y ->
      renderLayoutG cfg attr (CPosition $ tcx + tcbo, CPosition y) tcbs ss
    go (P.Para strs) =
      \y -> renderLayoutM cfg (CPosition tcx, CPosition y) tcs (inlinesToString strs)
    go x = return -- 一部のみをサポート

-- スライド表紙をRender
coverSlide :: Config -> [P.Block] -> [Double -> C.Render Double]
coverSlide cfg = map go
  where
    ag = alphaBackG cfg
    ttcy = textTitleCoverY cfg
    ttcs = textTitleCoverSize cfg
    tccy = textContextCoverY cfg
    tccs = textContextCoverSize cfg
    go :: P.Block -> Double -> C.Render Double
    go (P.Para [P.Image _ [P.Str "background"] (pngfile, _)]) =
      \y -> renderPngFit cfg ag pngfile >> return y
    go (P.Header 1 _ strs) =
      \y -> renderLayoutM cfg (CCenter, CPosition ttcy) ttcs (inlinesToString strs) >> return y
    go (P.Para strs) =
      \y -> renderLayoutM cfg (CCenter, CPosition tccy) tccs (inlinesToString strs) >> return y
    go _ = return -- 一部のみをサポート

updateCanvas :: Config -> G.DrawingArea -> IO ()
updateCanvas cfg canvas = do
  n <- queryCarettahState page
  s <- queryCarettahState slides
  win <- G.widgetGetDrawWindow canvas
  (width, height) <- G.widgetGetSize canvas
  G.renderWithDrawable win $
    renderSlide cfg s n width height
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

outputPDF :: Config -> String -> IO ()
outputPDF cfg pdf = do
  s <- queryCarettahState slides
  let iw = canvasW cfg
      ih = canvasH cfg
      dw = toDouble iw
      dh = toDouble ih
  C.withPDFSurface pdf dw dh $ flip C.renderWith . sequence_ $
    fmap (\a -> renderSlide cfg s a iw ih >> C.showPage) [0..(length s - 1)]

startPresentation :: Config -> Bool -> Double -> IO ()
startPresentation cfg wiiOn presenTime = do
  -- setup
  setWiiHandle wiiOn
  updateSpeechMinutes $ const presenTime
  -- start GUI
  void G.initGUI
  window <- G.windowNew
  canvas <- G.drawingAreaNew
  G.widgetSetSizeRequest window (canvasW cfg) (canvasH cfg)
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
                  loadMarkdown cfg md
                  curPage >> G.widgetQueueDraw canvas
        _   -> return ()
  void $ G.onDestroy window G.mainQuit
  void $ G.onExpose canvas $ const (updateCanvas cfg canvas >> return True)
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
                                     | b == cwiidBtnHome =
                                         do md <- queryCarettahState markdownFname
                                            loadMarkdown cfg md
                                            curPage >> G.widgetQueueDraw canvas
                                     | otherwise = return ()
                            go bs
                            return True) 50
  G.set window [G.containerChild G.:= canvas]
  G.widgetShowAll window
  updateStartTime
  updateRenderdTime
  G.mainGUI

loadMarkdown :: Config -> String -> IO ()
loadMarkdown cfg fn = do
  s <- readFile fn
  let blks = splitBlocks $ markdown s
      ncfg = newCfg cfg (head blks)
      z = zip (coverSlide ncfg:repeat (blockToSlide ncfg)) blks
  updateSlides $ const $ map (\p -> fst p . backgroundTop $ snd p) z
  where
    newCfg :: Config -> [P.Block] -> Config
    newCfg cfg blks =
      let blk = listToMaybe $ filter (\a -> a /= "") $ map go blks
      in maybe cfg (newCfg' cfg) blk
    newCfg' :: Config -> String -> Config
    newCfg' oldCfg blk =
      let r = CFG.parse (T.pack blk)
      in either (const oldCfg) (valueToCfg cfg) r
    valueToCfg :: Config -> CFG.Value -> Config
    valueToCfg oldCfg (CFG.Sections [
                          CFG.Section { CFG.sectionName  = fontNameP_name,
                                        CFG.sectionValue = CFG.Text fontNameP_value },
                          CFG.Section { CFG.sectionName  = fontNameM_name,
                                        CFG.sectionValue = CFG.Text fontNameM_value }
                          ]) =
      let fnp, fnm :: String
          fnp = if T.unpack fontNameP_name == "fontNameP"
                then T.unpack fontNameP_value
                else fontNameP oldCfg
          fnm = if T.unpack fontNameM_name == "fontNameM"
                then T.unpack fontNameM_value
                else fontNameM oldCfg
      in oldCfg { fontNameP = fnp, fontNameM = fnm }
    valueToCfg oldCfg _ = oldCfg
    go :: P.Block -> String
    go (P.CodeBlock ("", ["config"], _) ss) = ss
    go _ = ""

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
  loadMarkdown defaultConfig filen
  -- start
  case opts of
    (Options {optSlideInfo = True}) ->
      do s <- queryCarettahState slides
         putStrLn $ "Page: " ++ show (length s)
    (Options {optPdfOutput = Just pdf}) ->
      outputPDF defaultConfig pdf
    (Options {optWiimote = wiiOn, optTime = Just presenTime}) ->
      startPresentation defaultConfig wiiOn presenTime
    _ -> error "NOTREACHED"
  where
    defaultConfig :: Config
    defaultConfig = Config {
      canvasW   = 640,
      canvasH  = 480,
      alphaBackG = 0.3,
      textTitleCoverY = 170,
      textTitleCoverSize = 28,
      textContextCoverY = 300,
      textContextCoverSize = 26,
      textTitleY = 35,
      textTitleSize = 26,
      textContextX = 40,
      textContextY = 90,
      textContextSize = 18,
      textCodeBlockSize = 11,
      textCodeBlockOfs = 10,
      turtleSize = 40,
      waveSize = 20,
      waveCharMax = 42, -- xxx Should be calculated from waveSize
      fontNameP = "Noto Sans CJK JP",
      fontNameM = "Noto Sans Mono CJK JP"
    }
