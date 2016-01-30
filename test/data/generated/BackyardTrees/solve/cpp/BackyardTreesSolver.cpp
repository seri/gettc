#include <fstream>
#include <topcoder>
#include "BackyardTrees.cpp"
namespace tc = TopCoder;

int main(int argc, char const *argv[]) {
  try {
    std::ifstream ifs(argv[1]);
    int treeCount; tc::read(ifs, treeCount); tc::next(ifs);
    int width; tc::read(ifs, width); tc::next(ifs);
    int height; tc::read(ifs, height); tc::next(ifs);
    int distance; tc::read(ifs, distance);
    ifs.close();

    std::ofstream ofs(argv[2]);
    BackyardTrees solver;
    tc::show(ofs, solver.countWays(treeCount, width, height, distance));
    ofs.close();
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
  return 0;
}
