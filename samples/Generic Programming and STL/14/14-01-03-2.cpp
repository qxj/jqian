#include <iostream>
#include <iterator>
#include <algorithm>
#include <set>
using namespace std;

int main()
{
  const int N = 6;
  
  int A1[N] = {1, 3, 5, 7, 9, 11};
  int A2[N] = {1, 2, 3, 4, 5, 6};
  set<int> result;
  
  merge(A1, A1 + N, A2, A2 + N,
        inserter(result, result.begin()));
  
  copy(result.begin(), result.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  
  // ¿é¥X¬° "1 2 3 4 5 6 7 9 11"¡C
}
