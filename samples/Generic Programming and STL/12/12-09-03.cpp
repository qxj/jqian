#include <iostream>
#include <iterator>
#include <algorithm>
using namespace std;

int main()
{
  const int N = 10;
  int A[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  
  random_sample_n(A, A+N, ostream_iterator<int>(cout, " "), 4);
  cout << endl;
  // ���G�L�X�ӥi�ର 3 5 6 10�A
  //  �Υ��� 209 �ب䥦�i��ʤ��@�C  
}
