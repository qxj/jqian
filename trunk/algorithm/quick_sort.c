/* @(#)quick_sort.c
 * Time-stamp: <Julian Qian 2011-02-18 14:57:48>
 * Copyright 2011 Julian Qian
 * Version: $Id: quick_sort.c,v 0.0 2011/02/18 06:52:59 jqian Exp $
 */

/* The main idea behind QuickSort is ...
 * 1. Choose a data element in the array as the pivot
 * 2. Use the pivot to separate the data array into 2 partitions
 *    so that the left partition contains values less than the pivot
 *    and the right partition contains values greater than or equal
 *    to the pivot
 * 3. Apply QuickSort() recursively to sort the left and right
 *    partitions
 */

#include <stdio.h>

void swap(int* a, int i, int j){
    int tmp = a[i];
    a[i] = a[j];
    a[j] = tmp;
}

int partition(int* a, int low, int high){
    int pivot, pos;
    pos = low;
    pivot = a[pos];             /* select lowest element as pivot */
    for(int i=low+1; i<=high; ++i){
        if(a[i] < pivot){
            ++ pos;
            /* you can check whether pos equals i to avoid wasted swap. */
            swap(a, pos, i);    /* swap elements(>pivot) to right */
        }
    }
    swap(a, low, pos);          /* swap pivot to the correct position */
    return pos;                 /* return pivot's position */
}

void quick_sort(int* a, int low, int high){
    int pivot;
    if(low < high){
        pivot = partition(a, low, high);
        quick_sort(a, low, pivot-1);
        quick_sort(a, pivot+1, high);
    }
}

int main(int argc, char *argv[]){
    int a[] = {29, 4, 25, 7, 21, 1, 1, 9, 4, 24};
    int n = sizeof(a) / sizeof(a[0]);
    quick_sort(a, 0, n - 1);
    for (int i = 0; i < n; ++i) {
        printf("%d ", a[i]);
    }
    return 0;
}

