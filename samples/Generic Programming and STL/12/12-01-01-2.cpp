#include <algorithm>
#include <vector>
#include <list>
#include <cstring>
using namespace std;

int main()
{
  char A[] = "abcdefgh";
  vector<char> V(A, A + strlen(A));
  
  list<char> L;
  copy(V.begin(), V.end(), back_inserter(L));
  assert(equal(V.begin(), V.end(), L.begin()));
}
