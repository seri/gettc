import TopCoder
import System (getArgs)
import IO
import Solve
<% require 'topcoder/langs/haskell'; haskell = TopCoder::Langs::Haskell.new func, vars %>
getVars :: Parser (<%= haskell.var_types.join ', ' %>)
<%= haskell.input %>

main = do 
    args <- getArgs
    hIn <- openFile (head args) ReadMode
    contents <- hGetContents hIn
    case (parse getVars "variables" contents) of
        Left err -> print err
        Right (<%= haskell.var_names.join ', ' %>) -> do
            hOut <- openFile (head (tail args)) WriteMode
            hPutStr hOut $ show $ <%= func.name %> <%= haskell.var_names.join ' ' %>
            hClose hOut
    hClose hIn
