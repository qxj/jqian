#include <iostream>
#include <algorithm>
#include <vector>
#include <string>
#include <iterator>
using namespace std;

int main()
{
  vector<string> fruits;
  fruits.push_back("cherry");
  fruits.push_back("apple");
  fruits.push_back("peach");
  fruits.push_back("plum");
  fruits.push_back("pear");
  
  replace(fruits.begin(), fruits.end(),
          string("apple"), string("orange"));
  copy(fruits.begin(), fruits.end(),
       ostream_iterator<string>(cout, "\n"));
}
