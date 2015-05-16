<%
    engine = JavascriptEngine.new func, vars
%>exports.<%= func.name %> = function(<%= engine.arglist %>) {
    return <%= func.type.dumb_javascript %>;
};
