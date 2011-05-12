// @(#)pow3.cpp
// Time-stamp: <Julian Qian 2011-04-13 16:12:25>
// Copyright 2011 Julian Qian
// Version: $Id: pow3.cpp,v 0.0 2011/04/13 08:08:44 jqian Exp $

#include <cstdio>

template <int N>
class Pow3 {
public:
    static int const result = Pow3<N-1>::result + Pow3<N-2>::result;
};

template <>
class Pow3<1> {
public:
    static int const result = 1;
};

template <>
class Pow3<0> {
public:
    static int const result = 0;
};

int main(int argc, char *argv[]){
    printf("%d\n", Pow3<30>::result);
    return 0;
}


