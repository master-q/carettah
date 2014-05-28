import System.FilePath
import System.Environment
import System.Cmd
import Paths_carettah (getBinDir)

main :: IO ()
main = do
  as <- getArgs
  d <- getBinDir
  _ <- rawSystem (d </> "_carettah_main_") (as ++ ["+RTS", "-V0"])
  return ()
