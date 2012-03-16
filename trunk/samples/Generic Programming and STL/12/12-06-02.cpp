#include <functional>
#include <vector>
#include <iterator>
#include <algorithm>
using namespace std;

int main()
{
  vector<int> V;
  V.push_back(1);
  V.push_back(4);
  V.push_back(2);
  V.push_back(8);
  V.push_back(5);
  V.push_back(7);
  
  copy(V.begin(), V.end(), ostream_iterator<int>(cout, " "));
      // 輸出為 "1 4 2 8 5 7"
  
  vector<int>::iterator new_end =
          remove_if(V.begin(), V.end(),
                    compose1(bind2nd(equal_to<int>(), 0),
                             bind2nd(modulus<int>(), 2)));
  V.erase(new_end, V.end());

  copy(V.begin(), V.end(), ostream_iterator<int>(cout, " "));
      // 輸出為 "1 5 7"
}
