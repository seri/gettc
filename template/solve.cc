#include <topcoder>
using namespace TopCoder;
#include <fstream>
using namespace std;
<% require 'topcoder/langs/cpp' 
engine = TopCoder::Langs::Cpp.new func, vars %>
<%= engine.declare %>
{
    <%= engine.class.type_to_s func.type %> ret = <%= engine.class.dumb_value func.type %>;
    // Calculate ret here
    return ret;
}

int main(int argc, char const *argv[])
{
    try
    {
        ifstream ifs(argv[1]);
<%= engine.input %>
        ifs.close();

        ofstream ofs(argv[2]);
        show(ofs, <%= func.name %>(<%= engine.var_names.join ', ' %>));
        ofs.close();    
    }
    catch (exception &e)
    {
        cerr << e.what() << endl;       
    }
    return 0;
}
