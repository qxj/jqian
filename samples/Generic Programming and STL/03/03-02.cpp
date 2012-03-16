#include <iterator>
using namespace std;

template <class Node, class Reference, class Pointer>
struct node_wrap_base
  : public iterator<forward_iterator_tag, Node,
                    ptrdiff_t, Pointer, Reference>
{
  typedef node_wrap_base<Node, Node&, Node*>             iterator;
  typedef node_wrap_base<Node, const Node&, const Node*> const_iterator;
  Pointer ptr;
  
  node_wrap_base(Pointer p = 0) : ptr(p) {}
  node_wrap_base(const iterator& x) : ptr(x.ptr) {}
  
  Reference operator*() const { return *ptr; }
  Pointer operator->() const { return ptr; }
  
  void incr() { ptr = ptr->next; }
  
  bool operator==(const node_wrap_base& x) const { return ptr == x.ptr; }
  bool operator!=(const node_wrap_base& x) const { return ptr != x.ptr; }
};

template <class Node>
struct node_wrap : public node_wrap_base<Node, Node&, Node*>
{
  typedef node_wrap_base<Node, Node&, Node*> Base;
  node_wrap(Node* p = 0) : Base(p) {}
  node_wrap(const node_wrap<Node>& x) : Base(x) {}
  node_wrap& operator++()
    { incr(); return *this; }
  node_wrap  operator++(int)
    { node_wrap tmp = *this; incr(); return tmp; }
};

int main()
{
	node_wrap<int> nw;
}
