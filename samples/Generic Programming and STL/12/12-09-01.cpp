#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;

int main()
{
  const int N = 8;
  int A[] = {1, 2, 3, 4, 5, 6, 7, 8};
  random_shuffle(A, A + N);
  copy(A, A + N, ostream_iterator<int>(cout, " "));
  cout << endl;
  // ���G�L�X�ӥi�ର 7 1 6 3 2 5 4 8�A
  //  �Υ��� 40,319 �ب䥦�i��ʤ��@�C  
}
