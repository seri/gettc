#include <topcoder>
using namespace TopCoder;
#include "<%= prob.name %>.cpp"
#include <fstream>
<%
    engine = CppEngine.new func, vars
%>
int main(int argc, char const *argv[]) {
    try {
        ifstream ifs(argv[1]);
<%=
    engine.input.gsub(/^/, ' ' * 8)
%>
        ifs.close();

        ofstream ofs(argv[2]);
<%=
    engine.output.gsub(/^/, ' ' * 8)
%>
        ofs.close();
    } catch (exception &e) {
        cerr << e.what() << endl;
    }
    return 0;
}
