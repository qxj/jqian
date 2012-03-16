#include <algorithm>
#include <vector>
#include <slist>
#include <cstring>
using namespace std;

int main()
{
  char A[] = "abcdefgh";
  vector<char> V(A, A + strlen(A));
  
  slist<char> L(V.size());
  copy(V.begin(), V.end(), L.begin());
  assert(equal(V.begin(), V.end(), L.begin()));
}
