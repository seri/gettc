import TopCoder
import System (getArgs)
import IO
import Data.Char 
<% require 'topcoder/langs/haskell'
engine = TopCoder::Langs::Haskell.new func, vars %>
getVars :: Parser (<%= engine.var_types.join ', ' %>)
<%= engine.input %>

<%= func.name %> :: <%= engine.var_types.join ' -> ' %> -> <%= engine.class.type_to_s func.type %>
<%= func.name %> <%= engine.var_names.join ' ' %> = <%= engine.class.dumb_value func.type %>

main = do 
    args <- getArgs
    hIn <- openFile (head args) ReadMode
    contents <- hGetContents hIn
    case (parse getVars "variables" contents) of
        Left err -> print err
        Right (<%= engine.var_names.join ', ' %>) -> do
            hOut <- openFile (head (tail args)) WriteMode
            hPutStr hOut $ show $ <%= func.name %> <%= engine.var_names.join ' ' %>
            hClose hOut
    hClose hIn
