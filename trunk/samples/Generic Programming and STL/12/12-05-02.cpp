#include <vector>
#include <algorithm>
#include <iterator>
using namespace std;

int main()
{
  vector<double> V(2, 128.);
  fill_n(back_inserter(V), 3, 137.);
  assert(V.size() == 5 &&
         V[2] == 137 && V[3] == 137 && V[4] == 137);
}
