#include <iostream>
#include <algorithm>
using namespace std;

int main()
{
  int A[] = { 2, 0, 4, 6, 0, 3, 1, -7 };
  const int N = sizeof(A) / sizeof(int);
  
  int result = 0;
  count(A, A + N, 0, result);
  cout << "Number of zeros: " << result << endl;
}
