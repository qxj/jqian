#include <memory>
using namespace std;

class Int {
public:
  Int(int x) : val(x) {}
  // Int ¤£¨ã¦³ default constructor¡C
  int get() { return val; }
private:
  int val;
};

int main()
{
  int A1[] = {1, 2, 3, 4, 5, 6, 7};
  const int N = sizeof(A1) / sizeof(int);
  
  Int* A2 = (Int*) malloc(N * sizeof(Int));
  uninitialized_copy(A1, A1 + N, A2);
}
