import System.Environment (getArgs)
import System.IO
import qualified TopCoder as TC
import qualified CirclesCountry (leastBorders)

getVars :: TC.Parser ([Int], [Int], [Int], Int, Int, Int, Int)
getVars = do x <- TC.spaces >> (TC.parseList TC.parseInt) ; TC.spaces >> TC.next
             y <- TC.spaces >> (TC.parseList TC.parseInt) ; TC.spaces >> TC.next
             r <- TC.spaces >> (TC.parseList TC.parseInt) ; TC.spaces >> TC.next
             x1 <- TC.spaces >> TC.parseInt ; TC.spaces >> TC.next
             y1 <- TC.spaces >> TC.parseInt ; TC.spaces >> TC.next
             x2 <- TC.spaces >> TC.parseInt ; TC.spaces >> TC.next
             y2 <- TC.spaces >> TC.parseInt
             return (x, y, r, x1, y1, x2, y2)

main = do
    args <- getArgs
    hIn <- openFile (head args) ReadMode
    contents <- hGetContents hIn
    case (TC.parse getVars "parse variables" contents) of
        Left err -> print err
        Right (x, y, r, x1, y1, x2, y2) -> do
            hOut <- openFile (head (tail args)) WriteMode
            hPutStr hOut $ show $ CirclesCountry.leastBorders x y r x1 y1 x2 y2
            hClose hOut
    hClose hIn
