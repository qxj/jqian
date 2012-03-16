#include <vector>
#include <iterator>
#include <algorithm>
#include <string>
using namespace std;

int main()
{
  vector<string> V;
  
  V.push_back("one");
  V.push_back("");
  V.push_back("four");
  V.push_back("");
  V.push_back("");
  V.push_back("ten");
  
  remove_copy(V.begin(), V.end(),
              ostream_iterator<string>(cout, "\n"),
              string(""));
}
