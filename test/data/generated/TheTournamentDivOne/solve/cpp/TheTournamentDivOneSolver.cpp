#include <fstream>
#include <topcoder>
#include "TheTournamentDivOne.cpp"
namespace tc = TopCoder;

int main(int argc, char const *argv[]) {
  try {
    std::ifstream ifs(argv[1]);
    vector<int> points; tc::read(ifs, points); tc::next(ifs);
    int w; tc::read(ifs, w); tc::next(ifs);
    int d; tc::read(ifs, d);
    ifs.close();

    std::ofstream ofs(argv[2]);
    TheTournamentDivOne solver;
    tc::show(ofs, solver.find(points, w, d));
    ofs.close();
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
  return 0;
}
