#include <vector>
#include <algorithm>
#include <cstdlib>
using namespace std;

int main()
{
  vector<int> V(100);
  generate(V.begin(), V.end(), rand);
}
