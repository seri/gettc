#include <topcoder>
using namespace TopCoder; 
#include <fstream>
using namespace std;
<% require 'topcoder/langs/cpp' %>
int main(int argc, char *argv[])
{ 
    try
    { 
        <%= TopCoder::Langs::Cpp.type_to_s func.type %> output, result;
        ifstream ifs;
        
        ifs.open(argv[1]);
        read(ifs, output);
        ifs.close(); 

        ifs.open(argv[2]); 
        read(ifs, result);
        ifs.close();

        return same(output, result) ? 0 : 1;
    }
    catch (exception &e)
    {
        cerr << e.what() << endl;       
    }
    return -1;
}
