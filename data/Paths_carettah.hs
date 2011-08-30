module Paths_carettah where
import Data.Version

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return

version :: Version
version = Version {versionBranch = [0,0,0], versionTags = ["dummy"]}
