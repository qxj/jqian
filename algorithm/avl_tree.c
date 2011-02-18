/* @(#)avl_tree.c
 * Time-stamp: <Julian Qian 2011-02-18 15:14:04>
 * Copyright 2010 Julian Qian
 * Author: jqian@obama.min.veritas.com
 * Version: $Id: avl_tree.c,v 0.0 2010/04/18 04:35:04 jqian Exp $
 * Keywords:
 */

#include <stdio.h>
#include <stdlib.h>

#ifndef TRUE
#define TRUE 0
#endif /* TRUE */

#ifndef FALSE
#define FALSE 1
#endif /* FALSE */

#define ENTRY_T entry_t

typedef struct {
    int data;
} entry_t;

typedef struct avl_node_s {
    ENTRY_T entry;
    struct avl_node_s *left;
    struct avl_node_s *right;
    int balance;
} * avl_node_t;

#define AVL_NODE_ALLOC(node, entry)                     \
    do {                                                \
        node = calloc(1, sizeof(struct avl_node_s));    \
        (node)->entry = *entry;                         \
    } while (0)
#define AVL_NODE_UPDATE(node, entry)            \
    do {                                        \
        (node)->entry = *entry;                 \
    } while(0)
#define AVL_NODE_DEALLOC(node)                  \
    free(node)

int compare_entry(ENTRY_T *e1, ENTRY_T *e2){
    if(e1->data == e2->data){
        return 0;
    }else if(e1->data < e2->data){
        return -1;
    }else{
        return 1;
    }
}

/* build a AVL search tree for incremental ref entries */
void avl_rotate_left(avl_node_t tree, avl_node_t * newtree){
    *newtree = tree->right;
    tree->right = (*newtree)->left;
    (*newtree)->left = tree;
}

void avl_rotate_right(avl_node_t tree, avl_node_t* newtree){
    *newtree = tree->left;
    tree->left = (*newtree)->right;
    (*newtree)->right = tree;
}

void avl_left_balance(avl_node_t *tree, int *grow){
    avl_node_t rtree, ltree = (*tree)->left;
    switch(ltree->balance){
    case -1:                      /* left heigher */
    {
        (*tree)->balance = 0;
        ltree->balance = 0;
        avl_rotate_right(*tree, tree);
        *grow = 0;
    }
    break;
    case 0:                       /* balanced */
    {

    }
    break;
    case 1:                       /* right heigher */
    {
        rtree = ltree->right;
        switch(rtree->balance){  /* check right sub-tree's balance */
        case -1:
        {
            (*tree)->balance = 1;
            ltree->balance = 0;
        }
        break;
        case 0:
        {
            (*tree)->balance = 0;
            ltree->balance = 0;
        }
        break;
        case 1:
        {
            (*tree)->balance = 0;
            ltree->balance = -1;
        }
        break;
        }
        rtree->balance = 0;
        avl_rotate_left(ltree, &(*tree)->left); /* firstly, rotate left */
        avl_rotate_right(*tree, tree); /* then, rotate right */
        *grow = 0;
    }
    break;
    }
}

void avl_right_balance(avl_node_t* tree, int* grow){
    avl_node_t ltree, rtree = (*tree)->right;
    switch(rtree->balance){
    case 1:                       /* right heigher */
    {
        (*tree)->balance = 0;
        rtree->balance = 0;
        avl_rotate_left(*tree, tree);
        *grow = 0;
    }
    break;
    case 0:                       /* balanced */
    {

    }
    break;
    case -1:                      /* left heigher */
    {
        ltree = rtree->left;
        switch(ltree->balance){
        case 1:
        {

            (*tree)->balance = -1;
            rtree->balance = 0;
        }
        break;
        case 0:
        {
            (*tree)->balance = 0;
            rtree->balance = 0;
        }
        break;
        case -1:
        {
            (*tree)->balance = 0;
            rtree->balance = 1;
        }
        break;
        }
        ltree->balance = 0;
        avl_rotate_right(rtree, &(*tree)->right); /* firstly, rotate right */
        avl_rotate_left(*tree, tree); /* then, rotate left */
        *grow = 0;
    }
    break;
    }
}

int avl_insert(avl_node_t* tree,
               ENTRY_T* entry,
               int* grow){
    int ret = TRUE;
    if(*tree == NULL){            /* insert entry */
        AVL_NODE_ALLOC(*tree, entry);
        if(*tree == NULL){
            ret = FALSE;
            *grow = 0;
        } else {
            ret = TRUE;
            *grow = 1;
        }
    }else if(compare_entry(entry, &(*tree)->entry) < 0){ /* insert left */
        ret = avl_insert(&((*tree)->left), entry, grow);
        if(*grow){
            switch((*tree)->balance){
            case -1:
                avl_left_balance(tree, grow);
                break;
            case 0:
                (*tree)->balance = -1;
                break;
            case 1:
            {
                (*tree)->balance = 0;
                *grow = 0;
            }
            break;
            }
        }
    }else if(compare_entry(entry, &(*tree)->entry) > 0){ /* insert right */
        ret = avl_insert(&(*tree)->right, entry, grow);
        if(*grow){
            switch((*tree)->balance){
            case -1:
            {
                (*tree)->balance = 0;
                *grow = 0;
            }
            break;
            case 0:
                (*tree)->balance = 1;
                break;
            case 1:
                avl_right_balance(tree, grow);
                break;
            }
        }
    }else{                        /* same image offset, update it */
        AVL_NODE_UPDATE(*tree, entry);
    }
    return ret;
}

int avl_search(avl_node_t tree,
               ENTRY_T* entry){
    int cmp;
    if(tree == NULL) return -1; /* not found */
    if((cmp =compare_entry(entry, &tree->entry)) == 0){
        return 0;            /* found */
    }else if(cmp > 0){ /* lookup right branch */
        return avl_search(tree->right, entry);
    }else{                    /* left */
        return avl_search(tree->left, entry);
    }
}

void avl_free(avl_node_t *tree){
    if(*tree == NULL) return;
    if(&(*tree)->left != NULL) avl_free(&(*tree)->left);
    if((*tree)->right != NULL) avl_free(&(*tree)->right);
    if((*tree)->left == NULL && (*tree)->right == NULL) AVL_NODE_DEALLOC(*tree);
}

void avl_travel(avl_node_t *tree){
    if(tree == NULL) return;
    if(tree->left != NULL) avl_travel(tree->left);
    /* do something */
    printf("%d\t", tree->entry.data);
    fflush(stdout);
    if(tree->right != NULL) avl_travel(tree->right);
}

int main(int argc, char *argv[])
{
    int i, grow;
    avl_node_t root =  NULL;
    ENTRY_T entry;
    srandom(1234);
    printf("begin:\n");
    for(i = 0; i< 10; ++i){
        entry.data = random() % 1000;
        printf ("%d\t", entry.data);
        fflush(stdout);
        if(avl_insert(&root, &entry, &grow) != TRUE){
            fprintf(stderr, "malloc failed!\n");
            break;
        }
    }
    printf("\ntravel:\n");
    avl_travel(root);
    avl_free(&root);
    printf("\n");

    return 0;
}
