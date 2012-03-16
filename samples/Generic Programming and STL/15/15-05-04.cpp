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
  
  transform(M.begin(), M.end(), ostream_iterator<int>(cout, " "),
            select1st<map<int, double>::value_type>());
  cout << endl;
  // ¿é¥X¬° 1 33 47¡C
}
