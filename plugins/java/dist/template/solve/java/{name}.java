<%
    engine = JavaEngine.new(func, vars )
%>public class <%= prob.name %> {
<%=
    engine.declare.gsub(/^/, ' ' * 4)
%> {
        return <%= func.type.dumb_java %>;
    }
}
