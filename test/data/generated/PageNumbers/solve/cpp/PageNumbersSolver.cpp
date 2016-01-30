#include <fstream>
#include <topcoder>
#include "PageNumbers.cpp"
namespace tc = TopCoder;

int main(int argc, char const *argv[]) {
  try {
    std::ifstream ifs(argv[1]);
    int N; tc::read(ifs, N);
    ifs.close();

    std::ofstream ofs(argv[2]);
    PageNumbers solver;
    tc::show(ofs, solver.getCounts(N));
    ofs.close();
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
  return 0;
}
