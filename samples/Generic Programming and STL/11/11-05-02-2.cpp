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
  
  pair<const char*, const char*> result =
    mismatch(s1, s1 + N, s2, eq_nocase);
  
  if (result.first == s1 + N)
    cout << "The two strings do not differ" << endl;
  else {
    cout << "The strings differ starting at character "
         << result.first - s1 << endl;
    cout << "Trailing part of s1: " << result.first << endl;
    cout << "Trailing part of s2: " << result.second << endl;
  }
}
