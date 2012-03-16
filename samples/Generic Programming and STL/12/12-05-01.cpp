#include <vector>
#include <algorithm>
using namespace std;

int main()
{
  vector<double> V(4);
  fill(V.begin(), V.end(), 137);
  assert(V[0] == 137 && V[1] == 137 && V[2] == 137 && V[3] == 137);
}
