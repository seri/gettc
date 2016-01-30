import System.Environment (getArgs)
import System.IO
import qualified TopCoder as TC
import qualified BackyardTrees (countWays)

getVars :: TC.Parser (Int, Int, Int, Int)
getVars = do treeCount <- TC.spaces >> TC.parseInt ; TC.spaces >> TC.next
             width <- TC.spaces >> TC.parseInt ; TC.spaces >> TC.next
             height <- TC.spaces >> TC.parseInt ; TC.spaces >> TC.next
             distance <- TC.spaces >> TC.parseInt
             return (treeCount, width, height, distance)

main = do
    args <- getArgs
    hIn <- openFile (head args) ReadMode
    contents <- hGetContents hIn
    case (TC.parse getVars "parse variables" contents) of
        Left err -> print err
        Right (treeCount, width, height, distance) -> do
            hOut <- openFile (head (tail args)) WriteMode
            hPutStr hOut $ show $ BackyardTrees.countWays treeCount width height distance
            hClose hOut
    hClose hIn
