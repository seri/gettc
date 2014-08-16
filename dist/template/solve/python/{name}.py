<%
    engine = PythonEngine.new func, vars
%>def <%= func.name %>(<%= engine.arglist %>):
    return <%= func.type.dumb_python %>
