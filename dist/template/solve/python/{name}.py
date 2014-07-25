<%
    engine = PythonEngine.new func, vars
%>def <%= func.name %>(<%= engine.vars_list %>):
    return <%= func.type.dumb_python %>