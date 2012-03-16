#include <iostream>
#include <iterator>
#include <algorithm>
#include <functional>
#include <string>
using namespace std;

struct string_length_exceeds
{
  string_length_exceeds(int n) : limit(n) {}
  bool operator()(const string &s) const { return s.size() > limit; }
  
  int limit;
};

int main()
{
  string A[7] = {"oxygen", "carbon", "nitrogen", "iron",
                 "sodiym", "hydrogen", "silicon"};
  
  replace_if(A, A + 7,
             string_length_exceeds(6),
             "******");
  copy(A, A + 7, ostream_iterator<string>(cout, "\n"));
}
