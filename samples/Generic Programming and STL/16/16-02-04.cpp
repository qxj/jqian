#include <iostream>
#include <map>
#include <functional>
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
  multimap<const char*, int, ltstr> m;
  
  m.insert(make_pair("a", 1));
  m.insert(make_pair("c", 2));
  m.insert(make_pair("b", 3));
  m.insert(make_pair("b", 4));
  m.insert(make_pair("a", 5));
  m.insert(make_pair("b", 6));

  cout << "Number of elements with key a: " << m.count("a") << endl;  
  cout << "Number of elements with key b: " << m.count("b") << endl;  
  cout << "Number of elements with key c: " << m.count("c") << endl;  
  
  cout << "Elements in m: " << endl;
  for (multimap<const char*, int, ltstr>::iterator it = m.begin();
       it != m.end();
       ++it)
    cout << "  [" << (*it).first << ", " << (*it).second << "]" << endl;
}
