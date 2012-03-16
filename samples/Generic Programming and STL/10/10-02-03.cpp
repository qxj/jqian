#include <cassert>
#include <iterator>
#include <list>
using namespace std;

template <class ForwardIterator, class T>
ForwardIterator lower_bound(ForwardIterator first, ForwardIterator last,
                            const T& value) {
  iterator_traits<ForwardIterator>::difference_type len
        = distance(first, last);
  iterator_traits<ForwardIterator>::difference_type half;
  ForwardIterator middle;
  
  while (len > 0) {
    half = len / 2;
    middle = first;
    advance(middle, half);
    if (*middle < value) {
      first = middle;
      ++first;
      len = len - half - 1;
    } else
      len = half;
  }
  return first;
}

int main() {
  list<int> L;
  L.push_back(0);
  L.push_back(1);
  
  assert(distance(L.begin(), L.end()) == L.size());
}
