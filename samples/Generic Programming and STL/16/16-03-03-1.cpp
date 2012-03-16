#include <iostream>
#include <queue>
#include <cassert>
using namespace std;

int main() {
  priority_queue<int> Q;
  Q.push(1);
  Q.push(4);
  Q.push(2);
  Q.push(8);
  Q.push(5);
  Q.push(7);
  
  assert(Q.size() == 6);
  while (!Q.empty()) {
    cout << Q.top() << " ";
    Q.pop();
  }
  cout << endl;
  //  ��X�� 8 7 5 4 2 1
}
