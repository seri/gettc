#include <vector>
#include <string>
using namespace std;
<% require 'topcoder/langs/cpp'; cpp = TopCoder::Langs::Cpp.new func, vars %>
<%= cpp.declare %>
{
    return <%= cpp.class.dumb_value func.type %>;
}
