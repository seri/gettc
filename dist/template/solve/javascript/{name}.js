<%
    engine = JavascriptEngine.new func, vars
%>module.exports = {
    <%= func.name %> : function(<%= engine.arglist %>) {
        return <%= func.type.dumb_javascript %>;
    }
}
