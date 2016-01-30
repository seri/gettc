#include <vector>
#include <string>
using namespace std;

class <%= prob.name %> {
public:
<%= CppEngine.new(func, vars).declare.gsub(/^/, '  ') %> {
    return <%= func.type.dumb_cpp %>;
  }
};
