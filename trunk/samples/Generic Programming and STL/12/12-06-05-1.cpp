#include <iostream>
#include <iterator>
#include <algorithm>
#include <vector>
using namespace std;

int main() {
  vector<int> V;
  V.push_back(1);
  V.push_back(3);
  V.push_back(3);
  V.push_back(3);
  V.push_back(2);
  V.push_back(2);
  V.push_back(1);
  
  vector<int>::iterator new_end = unique(V.begin(), V.end());
  V.erase(new_end, V.end());
  copy(V.begin(), V.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  // ¿é¥X¬° "1 3 2 1"
}
