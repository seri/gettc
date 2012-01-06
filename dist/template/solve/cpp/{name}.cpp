#include <vector>
#include <string>
using namespace std;
<%
    engine = CppEngine.new func, vars
%>
<%=
    engine.declare
%> {
    return <%= func.type.dumb_cpp %>;
}
