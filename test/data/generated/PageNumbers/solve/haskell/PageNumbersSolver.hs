import System.Environment (getArgs)
import System.IO
import qualified TopCoder as TC
import qualified PageNumbers (getCounts)

getVars :: TC.Parser (Int)
getVars = do n <- TC.spaces >> TC.parseInt
             return (n)

main = do
    args <- getArgs
    hIn <- openFile (head args) ReadMode
    contents <- hGetContents hIn
    case (TC.parse getVars "parse variables" contents) of
        Left err -> print err
        Right (n) -> do
            hOut <- openFile (head (tail args)) WriteMode
            hPutStr hOut $ show $ PageNumbers.getCounts n
            hClose hOut
    hClose hIn
