#include <cstdlib>
#include <algorithm>
using namespace std;

int main()
{
  char* commands[] = {"uptime", "pwd", "ls"};
  const int N = sizeof(commands) / sizeof(char*);
  
  for_each(commands, commands + N, system);
}
