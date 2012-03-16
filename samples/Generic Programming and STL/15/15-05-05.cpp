#include <iostream>
#include <functional>
#include <algorithm>
#include <iterator>
#include <map>
using namespace std;

int main()
{
  map<int, double> M;
  M[1] = 0.3;
  M[47] = 0.8;
  M[33] = 0.1;
  
  transform(M.begin(), M.end(), ostream_iterator<double>(cout, " "),
            select2nd<map<int, double>::value_type>());
  cout << endl;
  // ¿é¥X¬° 0.3 0.1 0.8¡C
}
