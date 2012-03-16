#include <iostream>
#include <hash_set>
#include <cstring>
using namespace std;

struct eqstr
{
  bool operator()(const char* s1, const char* s2) const
  {
    return strcmp(s1, s2) == 0;
  }
};

typedef hash_multiset<const char*, hash<const char*>, eqstr>
        Hash_Multiset;

void lookup(Hash_Multiset& Set, const char* word)
{
  int n_found = Set.count(word);
  cout << "  " << word << ": "
       << n_found << " "
       << (n_found == 1 ? "instance" : "instances")
       << endl;
}

int main()
{
  Hash_Multiset Set;
  
  Set.insert("mango");
  Set.insert("kiwi");
  Set.insert("apple");
  Set.insert("pear");
  Set.insert("kiwi");
  Set.insert("mango");
  Set.insert("pear");
  Set.insert("mango");
  Set.insert("apricot");
  Set.insert("banana");
  Set.insert("mango");

  lookup(Set, "mango");
  lookup(Set, "apple");
  lookup(Set, "durian");
  
  // ¿é¥X¬°¡G
  //   mango: 4 instances
  //   apple: 1 instance
  //   durian: 0 instances
}
