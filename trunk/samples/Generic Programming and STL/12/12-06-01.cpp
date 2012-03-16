#include <iostream>
#include <vector>
#include <iterator>
#include <algorithm>
using namespace std;

template <class Container>
void print_container(const Container& C)
{
  cout << C.size() << " elements: ";
  copy(C.begin(), C.end(),
       ostream_iterator<typename Container::value_type>(cout, " "));
  cout << endl;
}

int main()
{
  const int A[9] = {3, 1, 4, 1, 5, 9, 2, 6, 5};
  
  vector<int> V(A, A + 9);
  print_container(V);
  
  vector<int>::iterator new_end = remove(V.begin(), V.end(), 1);
  print_container(V);
  
  V.erase(new_end, V.end());
  print_container(V);
}
