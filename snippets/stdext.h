// My std extensions

// copy elements to a new sequence only fulfill certain criteria
template<class In, class Out, class Pred>
Out copy_if(In first, In last, Out res, Pred p){
    while(first != last){
        if(p(*first)) *res++ = *first;
        first++;
    }
    return res;
}

// transform 2 sequence witout output to another sequence.
template <class In, class In2, class BinOp>
BinOp for_each(In first, In last, In2 first2, BinOp op){
    while(first != last) op(*first++, *first2++);
    return op;
}
