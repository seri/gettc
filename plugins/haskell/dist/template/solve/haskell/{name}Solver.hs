import System.Environment (getArgs)
import System.IO
import qualified TopCoder as TC
import qualified <%= prob.name %> (<%= func.name %>)
<%
    engine = HaskellEngine.new(func, vars )
%>
<%=
    engine.input
%>

main = do
    args <- getArgs
    hIn <- openFile (head args) ReadMode
    contents <- hGetContents hIn
    case (TC.parse getVars "parse variables" contents) of
        Left err -> print err
        Right (<%=
    temp = engine.vars.map do |var| var.name end
    temp.join(', ')
%>) -> do
            hOut <- openFile (head (tail args)) WriteMode
            hPutStr hOut $ show $ <%= prob.name %>.<%=
    engine.output
%>
            hClose hOut
    hClose hIn
