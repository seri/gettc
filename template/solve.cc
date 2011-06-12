#include <topcoder>
using namespace TopCoder;
#include <fstream>
using namespace std;

<%= cpp_method %>

int main(int argc, char const *argv[])
{
    try
    {
        ifstream ifs(argv[1]);
<%= cpp_input %>
        ifs.close();

        ofstream ofs(argv[2]);
<%= cpp_output %>
        ofs.close();    
    }
    catch (exception &e)
    {
        cerr << e.what() << endl;       
    }
    return 0;
}
