// treap begin
// 预开大数组存 TREAP_MAX_NODE 个treap节点，注意空间
// 可用new，delete分配空间，此时 TREAP_MAX_NODE 较小以存nullNode即可

#include <iostream>
#include <cstdlib>
#include <ctime>
#include <cstdio>

using namespace std;

class Treap // max heap
{
public :
    typedef int TreapK; // TreapK() and < and =
    enum { TREAP_MAX_NODE = 30000 };
    struct TreapNode
    {
        TreapK key;
        int prio, size, count;
        TreapNode *pcl, *pcr;
    };

    Treap() {
        if ( nullNode == 0 ) {
            nullNode = treapMemNode;
            nullNode->size = nullNode->count = 0;
        }
        if ( ++treapTot == 1 ) {
            treapMemTotNode = 1;
        }
        root = nullNode;
    }
    ~Treap() {
        --treapTot;
    }
    int contain( const TreapK & data ) {
        TreapNode * p = root;
        while ( p != nullNode ) {
            if ( data < p->key ) {
                p = p->pcl;
            }
            else if ( p->key < data ) {
                p = p->pcr;
            }
            else {
                return p->count;
            }
        }
        return 0;
    }
    bool empty() {
        return root == nullNode;
    }
    // order 1, 2, 3,
    bool getKth( int k, TreapK & data ) {
        if ( ( k < 1 ) || ( k > size() ) ) {
            return false;
        }
        TreapNode * p = root;
        while ( p != nullNode ) {
            if ( p->pcl->size >= k ) {
                p = p->pcl;
            }
            else if ( p->size - p->pcr->size >= k ) {
                data = p->key;
                return true;
            }
            else {
                p = p->pcr;
                k -= p->size - p->pcr->size;
            }
        }
        return false;
    }
    bool getMax( TreapK & data ) {
        if ( root == nullNode ) {
            return false;
        }
        TreapNode * p = root;
        while ( p->pcr != nullNode ) {
            p = p->pcr;
        }
        data = p->key;
        return true;
    }
    bool getMin( TreapK & data ) {
        if ( root == nullNode ) {
            return false;
        }
        TreapNode * p = root;
        while ( p->pcl != nullNode ) {
            p = p->pcl;
        }
        data = p->key;
        return true;
    }
    void inorder() {
        inorder( root );
    }
    void insert( const TreapK & data ) {
        pData = &data;
        insert( root );
    }
    // order 1, 2, 3,
    bool rank( const TreapK & data, int * prk ) {
        *prk = 1;
        TreapNode * p = root;
        while ( p != nullNode ) {
            if ( data < p->key ) {
                p = p->pcl;
            }
            else if ( p->key > data ) {
                *prk += p->pcl->size + p->count;
                p = p->pcr;
            }
            else {
                return true;
            }
        }
        return false;
    }
    void remove( const TreapK & data ) {
        if ( contain( data ) > 0 ) {
            pData = &data;
            remove( root );
        }
    }
    int size() {
        return root->size;
    }
private :
    void erase( TreapNode * & p ) {
        if ( p == nullNode ) {
            return;
        }
        if ( ( p->pcl == nullNode ) && ( p->pcr == nullNode ) ) {
            p = nullNode;
        }
        else if ( p->pcl == nullNode ) {
            p = p->pcr;
        }
        else if ( p->pcr == nullNode ) {
            p = p->pcl;
        }
        else {
            if ( p->pcl->prio > p->pcr->prio ) {
                rotL( p );
                erase( p->pcr );
            }
            else {
                rotR( p );
                erase( p->pcl );
            }
        }
    }
    void inorder( const TreapNode * p ) {
        if ( p == nullNode ) {
            return;
        }
        inorder( p->pcl );
        for ( int i = 0; i < p->count; ++i ) {
            printf( " %d", p->key );
        }
        inorder( p->pcr );
    }
    void insert( TreapNode * & p ) {
        if ( p == nullNode ) {
            p = treapMemNew( *pData );
            return;
        }
        ++(p->size);
        if ( *pData < p->key ) {
            insert( p->pcl );
            if ( p->pcl->prio  >  p->prio ) {
                rotL( p );
            }
        }
        else if ( p->key < *pData ) {
            insert( p->pcr );
            if ( p->pcr->prio  >  p->prio ) {
                rotR( p );
            }
        }
        else {
            ++(p->count);
        }
    }
    void remove( TreapNode * & p ) {
        if ( p == nullNode ) {
            return;
        }
        --(p->size);
        if ( *pData < p->key ) {
            remove( p->pcl );
        }
        else if ( p->key < *pData ) {
            remove( p->pcr );
        }
        else {
            if ( --(p->count) < 1 ) {
                erase( p );
            }
        }
    }
    void rotL( TreapNode * & p ) {
        TreapNode * t = p->pcl;
        p->pcl = t->pcr;
        t->pcr = p;
        p->size = p->count + p->pcl->size + p->pcr->size;
        t->size = t->count + t->pcl->size + p->pcr->size;
        p = t;
    }
    void rotR( TreapNode * & p ) {
        TreapNode * t = p->pcr;
        p->pcr = t->pcl;
        t->pcl = p;
        p->size = p->count + p->pcl->size + p->pcr->size;
        t->size = t->count + t->pcl->size + p->pcr->size;
        p = t;
    }
    TreapNode * root;
    const TreapK * pData;

    static TreapNode   treapMemNode[ TREAP_MAX_NODE ];
    static int         treapMemTotNode, treapTot;
    static TreapNode * nullNode;
    static TreapNode * treapMemNew( const TreapK & data ) {
        TreapNode * p = treapMemNode + treapMemTotNode++;
        p->count = 1;
        p->key   = data;
        p->pcl   = nullNode;
        p->pcr   = nullNode;
        p->prio  = rand();
        p->size  = 1;
        return p;
    }
};
Treap::TreapNode   Treap::treapMemNode[ Treap::TREAP_MAX_NODE ];
int                Treap::treapMemTotNode;
int                Treap::treapTot = 0;
Treap::TreapNode * Treap::nullNode = 0;

// treap end
