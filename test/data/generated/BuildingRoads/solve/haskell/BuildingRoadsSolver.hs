import System.Environment (getArgs)
import System.IO
import qualified TopCoder as TC
import qualified BuildingRoads (destroyRocks)

getVars :: TC.Parser ([String])
getVars = do field <- TC.spaces >> (TC.parseList TC.parseString)
             return (field)

main = do
    args <- getArgs
    hIn <- openFile (head args) ReadMode
    contents <- hGetContents hIn
    case (TC.parse getVars "parse variables" contents) of
        Left err -> print err
        Right (field) -> do
            hOut <- openFile (head (tail args)) WriteMode
            hPutStr hOut $ show $ BuildingRoads.destroyRocks field
            hClose hOut
    hClose hIn
