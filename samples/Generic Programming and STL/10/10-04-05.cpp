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
  const int N = 137;
  
  Int val(46);
  Int* A = (Int*) malloc(N * sizeof(Int));
  uninitialized_fill_n(A, N, val);
}
