#include <iostream>
#include <iterator>
#include <functional>
#include <algorithm>
#include <vector>
#include <cctype>
using namespace std;

inline bool eq_nocase(char c1, char c2) {
  return tolower(c1) == tolower(c2);
}

inline bool lt_nocase(char c1, char c2) {
  return tolower(c1) < tolower(c2);
}

int main()
{
  const char init[] = "The Standard Template Library";
  vector<char> V(init, init + sizeof(init));
  sort(V.begin(), V.end(), lt_nocase);
  copy(V.begin(), V.end(), ostream_iterator<char>(cout));
  cout << endl;
  vector<char>::iterator new_end = unique(V.begin(), V.end(), eq_nocase);
  copy(V.begin(), new_end, ostream_iterator<char>(cout));
  cout << endl;
  // ¿é¥X¬°¡G
  //   aaaabddeeehiLlmnprrrStTtTy
  // abdehiLmnprSty
}
