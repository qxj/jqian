#include <algorithm>
#include <cstdio>
#include <cctype>
using namespace std;

inline bool lt_nocase(char c1, char c2) {
  return toupper(c1) < toupper(c2);
}

int main()
{
  const char* s1 = "abc";
  const char* s2 = "ABC";
  const char* s3 = "abcDEF";
  const int N1 = 3, N2 = 3, N3 = 6;
  
  printf("%s < %s : %c\n",
         s1, s2,
         lexicographical_compare(s1, s1 + N1,
                                 s2, s2 + N2,
                                 lt_nocase) ? 't' : 'f');

  printf("%s < %s : %c\n",
         s2, s3,
         lexicographical_compare(s2, s2 + N2,
                                 s3, s3 + N3,
                                 lt_nocase) ? 't' : 'f');
}
// 輸出結果為：
// abc < ABC : f
// ABC < abcDEF : t
