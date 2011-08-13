import TopCoder
import System (getArgs)
import IO
import <%= prob.name %>
<% 
    engine = HaskellEngine.new func, vars 
%>
<%= 
    engine.input 
%> 

main = do 
    args <- getArgs
    hIn <- openFile (head args) ReadMode
    contents <- hGetContents hIn
    case (parse getVars "variables" contents) of
        Left err -> print err
        Right (<%= 
    temp = engine.vars.map do |var| var.name end
    temp.join(', ')
%>) -> do
            hOut <- openFile (head (tail args)) WriteMode
            hPutStr hOut $ show $ <%= 
    engine.output 
%>
            hClose hOut
    hClose hIn
