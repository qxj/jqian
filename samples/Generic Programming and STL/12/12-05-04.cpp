#include <vector>
#include <algorithm>
#include <cstdlib>
using namespace std;

int main()
{
  generate_n(ostream_iterator<int>(cout, "\n"), 100, rand);
}
