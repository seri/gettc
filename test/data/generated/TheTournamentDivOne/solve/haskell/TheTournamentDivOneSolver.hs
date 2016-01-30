import System.Environment (getArgs)
import System.IO
import qualified TopCoder as TC
import qualified TheTournamentDivOne (find)

getVars :: TC.Parser ([Int], Int, Int)
getVars = do points <- TC.spaces >> (TC.parseList TC.parseInt) ; TC.spaces >> TC.next
             w <- TC.spaces >> TC.parseInt ; TC.spaces >> TC.next
             d <- TC.spaces >> TC.parseInt
             return (points, w, d)

main = do
    args <- getArgs
    hIn <- openFile (head args) ReadMode
    contents <- hGetContents hIn
    case (TC.parse getVars "parse variables" contents) of
        Left err -> print err
        Right (points, w, d) -> do
            hOut <- openFile (head (tail args)) WriteMode
            hPutStr hOut $ show $ TheTournamentDivOne.find points w d
            hClose hOut
    hClose hIn
