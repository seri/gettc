#include <vector>
#include <string>
using namespace std;

class <%= prob.name %> {
public:
<%= engine = CppEngine.new func, vars
    engine.declare.gsub /^/, "      " %> {
        return <%= func.type.dumb_cpp %>;
    }
};
