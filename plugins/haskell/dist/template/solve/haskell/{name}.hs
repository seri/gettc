module <%= prob.name %> where 
<% 
    engine = HaskellEngine.new func, vars 
%>
<%= 
    engine.declare 
%>