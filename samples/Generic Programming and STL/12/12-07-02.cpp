#include <iostream>
#include <iterator>
#include <algorithm>
#include <vector>
using namespace std;

int main() {
  vector<int> V;
  V.push_back(0);
  V.push_back(1);
  V.push_back(2);
  copy(V.begin(), V.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
                // ��X�� "0 1 2"
  reverse_copy(V.begin(), V.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
                // ��X�� "2 1 0"
}
