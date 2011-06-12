import System (getArgs)
import IO
import Data.Char

<%= hs_method %>

main = do 
    args <- getArgs
    hIn <- openFile (head args) ReadMode
<%= hs_input %>
    hClose hIn

    hOut <- openFile (head (tail args)) WriteMode
<%= hs_output %>
    hClose hOut
