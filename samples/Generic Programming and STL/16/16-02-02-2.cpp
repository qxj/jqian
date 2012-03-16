#include <iostream>
#include <map>
#include <string>
using namespace std;

int main() {
  map<string, int> M;
  
  M.insert(make_pair("A", 17));
  M.insert(make_pair("B", 74));
  
  if (M.find("Z") == M.end())
    cout << "Not found: Z" << endl;

                   // 安插新值於此 map 中
  pair<map<string, int>::iterator, bool> p = M.insert(make_pair("C", 4));
  assert(p.second);
  
                   // 改變與 B 關聯的值
  cout << "Value associated with B: " << p.first->second << endl;
  p.first->second = 7;
  cout << "Value associated with B: " << p.first->second << endl;
}
