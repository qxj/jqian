#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;

int main()
{
  const int N = 10;
  const int n = 4;
  int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  int B[n];
  
  random_sample(A, A+N, B, B+n);
  copy(B, B + n, ostream_iterator<int>(cout, " "));
  cout << endl;
  // ���G�L�X�ӥi�ର 1 6 3 5�A
  //  �Υ��� 5039 �ب䥦�i��ʤ��@�C  
}
