#include <iostream>
#include <iterator>
#include <algorithm>
#include <string>
using namespace std;

int main()
{
  string s = "This is a test.\n";
  copy(s.begin(), s.end(), ostreambuf_iterator<char>(cout));
}
