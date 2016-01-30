#include <fstream>
#include <topcoder>
#include "BuildingRoads.cpp"
namespace tc = TopCoder;

int main(int argc, char const *argv[]) {
  try {
    std::ifstream ifs(argv[1]);
    vector<string> field; tc::read(ifs, field);
    ifs.close();

    std::ofstream ofs(argv[2]);
    BuildingRoads solver;
    tc::show(ofs, solver.destroyRocks(field));
    ofs.close();
  } catch (std::exception &e) {
    std::cerr << e.what() << std::endl;
  }
  return 0;
}
