#include <cstdio>
#include <cstring>
#include <algorithm>
using namespace std;

int main()
{
  const char S1[] = "Hello, world!";
  const char S2[] = "world";
  const int N1 = strlen(S1);
  const int N2 = strlen(S2);
  
  const char* p = search(S1, S1 + N1, S2, S2 + N2);
  if (p != S1 + N1)
    printf("Found substring \"%s\" at charactor %d of string \"%s\".\n",
           S2, p - S1, S1);
  else
    printf("Couldn't find substring.\n");
}
