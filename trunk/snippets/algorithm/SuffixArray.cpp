// txt[ 0..n ), txt[ 0..n-1 ] > 0, txt[ n ] == 0
// sa[ 1..n ] = [ 0..n-1 ], sa[ 0 ] = n
// rk[ 0..n-1 ] = [ 1..n ], rk[ n ] = 0
void Da( const unsigned int * txt, int * pn, int * sa, int * rk, int * ht, int * tot, int totSize ) {
    int *x = rk, *y = ht, *txy, lastRk = totSize - 1, i, j, len, n;
    for( i = 0; i <= lastRk; ++i ){
        tot[ i ] = 0;
    }
    for( n = 0; txt[ n ]; ++n ){
        ++tot[ txt[ n ] ];
    }
    ++tot[ txt[ *pn = n ] ];
    for( i = 1; i <= lastRk; ++i ){
        tot[ i ] += tot[ i - 1 ];
    }
    for( i = n; i >= 0; --i ){
        sa[ --tot[ txt[ i ] ] ] = i;
    }
    x[ sa[ 0 ] ] = lastRk = 0;
    for( i = 1; i <= n; ++i ){
        if( txt[ sa[ i - 1 ] ] != txt[ sa[ i ] ] ){
            ++lastRk;
        }
        x[ sa[ i ] ] = lastRk;
    }
    for( len = 1; lastRk < n; len <<= 1 ){
        j = -1;
        for( i = n - len + 1; i <= n; ++i ){
            y[ ++j ] = i;
        }
        for( i = 0; i <= n; ++i ){
            if( sa[ i ] >= len ){
                y[ ++j ] = sa[ i ] - len;
            }
        }
        for( i = 0; i <= lastRk; ++i ){
            tot[ i ] = 0;
        }
        for( i = 0; i <= n; ++i ){
            ++tot[ x[ y[ i ] ] ];
        }
        for( i = 1; i <= lastRk; ++i ){
            tot[ i ] += tot[ i - 1 ];
        }
        for( i = n; i >= 0; --i ){
            sa[ --tot[ x[ y[ i ] ] ] ] = y[ i ];
        }
        txy = x;
        x   = y;
        y   = txy;
        x[ sa[ 0 ] ] = lastRk = 0;
        for( i = 1; i <= n; ++i ){
            x[ sa[ i ] ] = ( ( y[ sa[ i - 1 ] ] == y[ sa[ i ] ] ) &&
                             ( y[ sa[ i - 1 ] + len ] == y[ sa[ i ] + len ] )
                ) ? lastRk : ++lastRk;
        }
    }
    for( i = 0; i <= n; ++i ){
        rk[ i ] = x[ i ];
    }
    for( ht[ 0 ] = len = i = 0; i < n; ++i ){
        if( len > 0 ){
            --len;
        }
        j = sa[ rk[ i ] - 1 ];
        while( txt[ i + len ] == txt[ j + len ] ){
            ++len;
        }
        ht[ rk[ i ] ] = len;
    }
    return;
}
