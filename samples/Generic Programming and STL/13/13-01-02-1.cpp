#include <iostream>
#include <algorithm>
#include <cctype>
using namespace std;

inline bool lt_nocase(char c1, char c2) {
  return tolower(c1) < tolower(c2);
}

int main()
{
  char A[] = "fdBeACFDbEac";
  const int N = sizeof(A) - 1;
  stable_sort(A, A+N, lt_nocase);
  cout << A << endl;
  // 輸出結果為 "AaBbCcdDeEfF"
}
