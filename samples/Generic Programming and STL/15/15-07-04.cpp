#include <iostream>
#include <functional>
#include <iterator>
#include <algorithm>
#include <vector>
using namespace std;

int main() {
  int A1[5] = {1, 2, 3, 4, 5};
  int A2[5] = {1, 1, 2, 3, 5};
  int A3[5] = {1, 4, 1, 5, 9};
  
  vector<vector<int> > V;
  V.push_back(vector<int>(A1, A1 + 5));
  V.push_back(vector<int>(A2, A2 + 5));
  V.push_back(vector<int>(A3, A3 + 5));
  
  int indices[3] = {0, 2, 4};
  
  int& (vector<int>::*extract)(vector<int>::size_type);
  extract = &vector<int>::operator[];
  transform(V.begin(), V.end(), indices,
            ostream_iterator<int>(cout, " "),
            mem_fun_ref(extract));
  cout << endl;
}
