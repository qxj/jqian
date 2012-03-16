#include <algorithm>
#include <cstdio>
using namespace std;

int main()
{
  const char* WS = " \t\n";
  const int n_WS = strlen(WS);
  
  char* s1 = "This sentence contains five words.";
  char* s2 = "OneWord";
  
  
  char* end1 = find_first_of(s1, s1 + strlen(s1),
                             WS, WS + n_WS);
  char* end2 = find_first_of(s2, s2 + strlen(s2),
                             WS, WS + n_WS);
  
  printf("First word of s1: %.*s\n", end1 - s1, s1);
  printf("First word of s2: %.*s\n", end2 - s2, s2);
}
