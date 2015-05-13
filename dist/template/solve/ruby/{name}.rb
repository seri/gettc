<%
    engine = RubyEngine.new func, vars
%>class <%= prob.name %>
    def <%= func.name %>(<%= engine.arglist %>)
        return <%= func.type.dumb_ruby %>
    end
end
