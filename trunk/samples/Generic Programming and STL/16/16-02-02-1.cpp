#include <iostream>
#include <map>
#include <algorithm>
#include <iterator>
#include <cstring>
using namespace std;

struct ltstr
{
  bool operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) < 0;
  }
};

int main()
{
  map<const char*, int, ltstr> days;
  
  days["january"] = 31;
  days["february"] = 28;
  days["march"] = 31;
  days["april"] = 30;
  days["may"] = 31;
  days["june"] = 30;
  days["july"] = 31;
  days["august"] = 31;
  days["september"] = 30;
  days["october"] = 31;
  days["november"] = 30;
  days["december"] = 31;
  
  cout << "june -> " << days["june"] << endl;
  map<const char*, int, ltstr>::iterator cur = days.find("june");
  map<const char*, int, ltstr>::iterator prev = cur;
  map<const char*, int, ltstr>::iterator next = cur;
  ++next;
  --prev;
  cout << "Previous (in alphabetical order) is "
       << (*prev).first << endl;
  cout << "Next (in alphabetical order) is "
       << (*next).first << endl;
}
