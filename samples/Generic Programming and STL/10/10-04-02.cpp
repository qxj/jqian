#include <memory>
using namespace std;

class Int {
public:
  Int(int x) : val(x) {}
  int get() { return val; }
private:
  int val;
};

int main()
{
  Int A[] = { Int(1), Int(2), Int(3) };
  
  destroy(A, A + 3);
  construct(A + 0, Int(10));
  construct(A + 1, Int(11));
  construct(A + 2, Int(12));
}


