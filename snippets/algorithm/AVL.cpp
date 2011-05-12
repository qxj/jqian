#include <iostream>
#include <cstdio>

using namespace std;

template< class T >
class AVL
{
public :
    struct Node
    {
        Node() {
            h = 0;
            lc = rc = NULL;
        }
        void upH() {
            int a = ( (lc==NULL) ? (-1) : (lc->h) );
            int b = ( (rc==NULL) ? (-1) : (rc->h) );
            h = ( a < b ? b : a ) + 1;
        }
        int cmpLR() {
            return ((lc!=NULL)&&(rc!=NULL)) ? ( (lc->h) - (rc->h) ) : 0;
        }
        T data;
        int h;
        Node *lc, *rc;
    };

    AVL() {
        root = null = new Node();
    }
    ~AVL() {
        clear();
        delete null;
    }
    void clear() {
        clear( &root );
    }
    bool find( const T &d ) {
        Node *p = root;
        while ( p != null ) {
            if ( p->data < d ) {
                p = p->rc;
            }
            else if ( d < p->data ) {
                p = p->lc;
            }
            else {
                return true;
            }
        }
        return false;
    }
    bool insert( const T &d ) {
        return insert( &root, d );
    }
    void output() {
        output( root );
    }
private :
    void rotL( Node **pp ) {
        Node *tl = (*pp)->lc;
        (*pp)->lc = tl->rc;
        tl->rc = (*pp);
        (*pp) = tl;
        tl->rc->upH();
        tl->upH();
    }
    void rotR( Node **pp ) {
        Node *tr = (*pp)->rc;
        (*pp)->rc = tr->lc;
        tr->lc = (*pp);
        (*pp) = tr;
        tr->lc->upH();
        tr->upH();
    }
    void clear( Node **pp ) {
        if ( (*pp) == null ) return;
        clear( &((*pp)->lc) );
        clear( &((*pp)->rc) );
        delete (*pp);
        (*pp) = null;
    }
    bool insert( Node **pp, const T &d ) {
        if ( (*pp) == null ) {
            (*pp) = new Node();
            (*pp)->data = d;
            (*pp)->lc = (*pp)->rc = null;
            (*pp)->upH();
            return true;
        }
        // if ( (*pp)->data < d ) {  // 不允许重复
        if ( (*pp)->data <= d ) {    // 允许重复
            if ( insert( &((*pp)->rc), d ) ) {
                (*pp)->upH();
                if ( (*pp)->cmpLR() < -1 ) {
                    if ( d < (*pp)->rc->data ) {
                        rotL( &((*pp)->rc) );
                    }
                    rotR( pp );
                }
                return true;
            }
        }
        if ( d < (*pp)->data ) {
            if ( insert( &((*pp)->lc), d ) ) {
                (*pp)->upH();
                if ( (*pp)->cmpLR() > 1 ) {
                    if ( (*pp)->lc->data < d ) {
                        rotR( &((*pp)->lc) );
                    }
                    rotL( pp );
                }
                return true;
            }
        }
        return false; // 已经存在
    }
    void output( Node *p ) {
        if ( p == null ) return;
        output( p->lc );
        // output p data
        output( p->rc );
    }

    Node *root, *null;
};

