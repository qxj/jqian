#include <iostream>
#include <functional>
#include <algorithm>
#include <cstdlib>
#include <cstring>
#include <cassert>
using namespace std;

int main()
{
  char str[] = "The first line\nThe second line";
  int len = strlen(str);
  
  const char* wptr = find_if(str, str + len,
                             compose2(logical_or<bool>(),
                                      bind2nd(equal_to<char>(), ' '),
                                      bind2nd(equal_to<char>(), '\n')));
  assert(wptr == str + len || *wptr == ' ' || *wptr == '\n');
}