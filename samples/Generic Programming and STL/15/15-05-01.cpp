#include <iostream>
#include <functional>
#include <cassert>
using namespace std;

int main()
{
  int x = 137;
  identity<int> id;
  assert(x == id(x));
}
