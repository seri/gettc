#include <fstream>
#include <topcoder>
#include "<%= prob.name %>.cpp"
namespace tc = TopCoder;
<%
    engine = CppEngine.new func, vars
%>
int main(int argc, char const *argv[]) {
    try {
    std::ifstream ifs(argv[1]);
<%=
    engine.input.gsub(/^/, ' ' * 8)
%>
    ifs.close();

    std::ofstream ofs(argv[2]);
    <%= prob.name %> solver;
<%=
    engine.output.gsub(/^/, ' ' * 8)
%>
    ofs.close();
    } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
    }
    return 0;
}
