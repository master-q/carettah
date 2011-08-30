module WrapPaths (wrapGetDataFileName, wrapVersion) where
import Data.Version
import Paths_carettah (getDataFileName, version)

wrapGetDataFileName :: FilePath -> IO FilePath
wrapGetDataFileName = getDataFileName

wrapVersion :: Version
wrapVersion = version
