// @(#)deduced.cpp
// Time-stamp: <Julian Qian 2011-04-13 13:55:56>
// Copyright 2011 Julian Qian
// Version: $Id: deduced.cpp,v 0.0 2011/04/13 05:54:04 jqian Exp $

template <typename T>
void f1(T*);

template <typename E, int N>
void f2(E(&)[N]);

template <typename T1, typename T2, typename T3>
void f3(T1(T2::*)(T3*));

class S {
public:
    void f(double*);
};

void g(int *** ppp){
    bool b[42];
    f1(ppp);
    f2(b);
    f3(&S::f);
}

