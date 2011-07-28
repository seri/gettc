#include <topcoder>
using namespace TopCoder;
#include "solve.hh"
#include <fstream>
<% require 'topcoder/langs/cpp'; cpp = TopCoder::Langs::Cpp.new func, vars; %>
int main(int argc, char const *argv[])
{
    try
    {
        ifstream ifs(argv[1]);
<%= cpp.input %>
        ifs.close();

        ofstream ofs(argv[2]);
        show(ofs, <%= func.name %>(<%= cpp.var_names.join ', ' %>));
        ofs.close();    
    }
    catch (exception &e)
    {
        cerr << e.what() << endl;       
    }
    return 0;
}
