#include <iostream>
#include <cstring>
#include <cctype>
#include <algorithm>
using namespace std;

inline bool eq_nocase(char c1, char c2) {
  return toupper(c1) == toupper(c2);
}

int main()
{
  const char* s1 = "This is a Test";
  const char* s2 = "This is a test";
  const int N = strlen(s1);
  
  if (equal(s1, s1 + N, s2, eq_nocase))
    cout << "Equal" << endl;
  else
    cout << "Not equal" << endl;
}
