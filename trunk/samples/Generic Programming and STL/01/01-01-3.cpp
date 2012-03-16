// VC6[o] BCB4[o] GCC295[o]

#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
#include <iterator>

using namespace std;

struct strtab_cmp
{
  typedef vector<char>::iterator strtab_iterator;
  
  bool operator()(const pair<strtab_iterator, strtab_iterator>& x,
                  const pair<strtab_iterator, strtab_iterator>& y) const {
    return lexicographical_compare(x.first, x.second, y.first, y.second);
  }
};

struct strtab_print
{
  ostream& out;
  strtab_print(ostream& os) : out(os) {}
  
  typedef vector<char>::iterator strtab_iterator;
  void operator()(const pair<strtab_iterator, strtab_iterator>& s) const {
    copy(s.first, s.second, ostream_iterator<char>(out));
  }
};

int main()
{
  vector<char> strtab;  // 產生字串表
  char c;
  while (cin.get(c)) {
    strtab.push_back(c);
  }
                        // 剖析字串表予以斷行
  typedef vector<char>::iterator strtab_iterator;
  vector<pair<strtab_iterator, strtab_iterator> > lines;
  strtab_iterator start = strtab.begin();
  while (start != strtab.end()) {
    strtab_iterator next = find(start, strtab.end(), '\n');
    if (next != strtab.end())
      ++next;
    lines.push_back(make_pair(start, next));
    start = next;
  }
                        // 對於行的 vector 作排序
  sort(lines.begin(), lines.end(), strtab_cmp());
  
                        // 將排序結果寫到標準輸出上
  for_each(lines.begin(), lines.end(), strtab_print(cout));
}
