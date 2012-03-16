#include <iostream>
#include <hash_map>
#include <cstring>
using namespace std;


struct eqstr
{
  bool operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) == 0;
  }
};

typedef hash_multimap<const char*, int, hash<const char*>, eqstr>
        map_type;

void lookup(const map_type& Map, const char* str)
{
  cout << "  " << str << ": ";
  pair<map_type::const_iterator, map_type::const_iterator> p
    = Map.equal_range(str);
  for (map_type::const_iterator i = p.first; i != p.second; ++i)
    cout << (*i).second << " ";
  cout << endl;
}

int main()
{
  map_type M;
  
  M.insert(map_type::value_type("H", 1));
  M.insert(map_type::value_type("H", 2));
  M.insert(map_type::value_type("C", 12));
  M.insert(map_type::value_type("C", 13));
  M.insert(map_type::value_type("O", 16));
  M.insert(map_type::value_type("O", 17));
  M.insert(map_type::value_type("O", 18));
  M.insert(map_type::value_type("I", 127));
  
  lookup(M, "I");
  lookup(M, "O");
  lookup(M, "Rn");
  
  // ¿é¥X¬°¡G
  //   I: 127
  //   O: 16 18 17
  //   Rn:
}
