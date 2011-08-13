import java.io.*;
import java.util.*;
import org.topcoder.*;
<%
    engine = JavaEngine.new func, vars 
%>
public class <%= prob.name %>Runner {
    public static void main(String[] args) {
    try {
        TopcoderReader reader = new TopcoderReader(new FileReader(args[0]));
<%= 
    engine.input.gsub(/^/, ' ' * 8) 
%>
        reader.close();

        <%= prob.name %> solver = new <%= prob.name %>();
        TopcoderWriter writer = new TopcoderWriter(new FileWriter(args[1]));
<%=
    engine.output.gsub(/^/, ' ' * 8)
%>
        writer.close();
    } catch (IOException ioe) {
        ioe.printStackTrace(System.err);
    }
    }    
}