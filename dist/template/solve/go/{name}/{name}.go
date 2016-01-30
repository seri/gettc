package <%= prob.name %>
<%
    engine = GoEngine.new(func, vars)
%>
func <%= engine.func_name %>(<%= engine.declare.join(", ") %>) <%= func.type.to_go %> {
    return <%= func.type.dumb_go %>
}
