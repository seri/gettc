#include <fstream>
#include <topcoder>
#include "CirclesCountry.cpp"
namespace tc = TopCoder;

int main(int argc, char const *argv[]) {
  try {
    std::ifstream ifs(argv[1]);
    vector<int> X; tc::read(ifs, X); tc::next(ifs);
    vector<int> Y; tc::read(ifs, Y); tc::next(ifs);
    vector<int> R; tc::read(ifs, R); tc::next(ifs);
    int x1; tc::read(ifs, x1); tc::next(ifs);
    int y1; tc::read(ifs, y1); tc::next(ifs);
    int x2; tc::read(ifs, x2); tc::next(ifs);
    int y2; tc::read(ifs, y2);
    ifs.close();

    std::ofstream ofs(argv[2]);
    CirclesCountry solver;
    tc::show(ofs, solver.leastBorders(X, Y, R, x1, y1, x2, y2));
    ofs.close();
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
  return 0;
}
