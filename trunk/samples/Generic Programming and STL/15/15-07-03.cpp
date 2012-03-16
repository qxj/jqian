#include <iostream>
#include <functional>
#include <iterator>
#include <algorithm>
#include <vector>
using namespace std;

struct Operation {
  virtual double eval(double) = 0;
};

struct Square : public Operation {
  double eval(double x) { return x * x; }
};

struct Negate : public Operation {
  double eval(double x) { return -x; }
};

int main()
{
  vector<Operation*> operations;
  vector<double> operands;
  
  operations.push_back(new Square);
  operations.push_back(new Square);
  operations.push_back(new Negate);
  operations.push_back(new Negate);
  operations.push_back(new Square);
  
  operands.push_back(1);
  operands.push_back(2);
  operands.push_back(3);
  operands.push_back(4);
  operands.push_back(5);
  
  transform(operations.begin(), operations.end(),
            operands.begin(),
            ostream_iterator<double>(cout, "\n"),
            mem_fun(&Operation::eval));
}
