#include <iostream>
#include <functional>
#include <algorithm>
using namespace std;

int main()
{
  int A[] = { 2, 0, 4, 6, 0, 3, 1, -7 };
  const int N = sizeof(A) / sizeof(int);
  
  cout << "Number of zeros: " 
       << count_if(A, A + N, bind2nd(equal_to<int>(), 0)) 
       << endl;
}
