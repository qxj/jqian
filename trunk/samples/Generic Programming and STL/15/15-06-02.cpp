#include <iostream>
#include <functional>
using namespace std;

int main()
{
  subtractive_rng R;
  for (int i = 0; i < 20; ++i)
    cout << R(5) << ' ';
  cout << endl;
}
// ¿é¥X¬° 3 2 3 2 4 3 1 1 2 2 0 3 4 4 4 4 2 1 0 0
