// VC6[o] BCB4[o] GCC295[o]

#include <iostream>
#include <vector>
#include <algorithm>
#include <string>

using namespace std;

int main() {
  vector<string> V;
  string tmp;
	
  while (getline(cin, tmp))
    V.push_back(tmp);
	
  sort(V.begin(), V.end());
  copy(V.begin(), V.end(), ostream_iterator<string>(cout, "\n"));
}
