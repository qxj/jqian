#include <iostream>
#include <algorithm>
#include <iterator>
#include <string>
#include <vector>
#include <functional>
#include <cassert>
using namespace std;

int main()
{
  vector<string> fruits;
  fruits.push_back("apple");
  fruits.push_back("banana");
  fruits.push_back("pear");
  fruits.push_back("grapefruit");
  fruits.push_back("cherry");
  fruits.push_back("orange");
  fruits.push_back("watermelon");
  fruits.push_back("mango");

  sort(fruits.begin(), fruits.end(),
       greater<string>());
  assert(is_sorted(fruits.begin(), fruits.end(),
         greater<string>()));
  
  copy(fruits.begin(), fruits.end(),
       ostream_iterator<string>(cout, "\n"));
}
