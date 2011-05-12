// MergeTree begin

// T(),  T < T, T = T
// ensure 2^NL >  ri
// [ le, ri ]

template<class T, int NL>
class MergeTree
{
public :
    void init( const T d[], int le, int ri, int dep = 0, int v = 1 ) {
        left[ v ]  = le;
        right[ v ] = ri;
        if ( le == ri ) {
            data[ dep ][ le ] = d[ le ];
            return;
        }
        int mid = ( le + ri ) / 2;
        init( d, le, mid, dep + 1, v + v );
        init( d, mid + 1, ri, dep + 1, v + v + 1 );
        int i = le, j = mid + 1, k = le;
        while ( ( i <= mid ) && ( j <= ri ) ) {
            if ( data[ dep + 1 ][ i ] < data[ dep + 1 ][ j ] ) {
                data[ dep ][ k++ ] = data[ dep + 1 ][ i++ ];
            }
            else {
                data[ dep ][ k++ ] = data[ dep + 1 ][ j++ ];
            }
        }
        while ( i <= mid ) {
            data[ dep ][ k++ ] = data[ dep + 1 ][ i++ ];
        }
        while ( j <= ri ) {
            data[ dep ][ k++ ] = data[ dep + 1 ][ j++ ];
        }
    }
    int getCountBelow( int le, int ri, const T & da, int dep = 0, int v = 1 ) {
        if ( ( ri < left[ v ] ) || ( right[ v ] < le ) ) {
            return 0;
        }
        if ( ( le <= left[ v ] ) && ( right[ v ] <= ri ) ) {
            int low = left[ v ], high = right[ v ], mid, ans = left[ v ] - 1;
            while ( low <= high ) {
                mid = ( low + high ) >> 1;
                if ( data[ dep ][ mid ] < da ) {
                    low = mid + 1;
                    if ( ans < mid ) {
                        ans = mid;
                    }
                }
                else {
                    high = mid - 1;
                }
            }
            return ans - left[ v ] + 1;
        }
        return getCountBelow( le, ri, da, dep + 1, v + v ) +
            getCountBelow( le, ri, da, dep + 1, v + v + 1 );
    }
    // k = 1, 2,  , ri - le + 1, le <= ri
    T getKth( int le, int ri, int k ) {
        int low = left[ 1 ], high = right[ 1 ], mid, rk, ans = left[ 1 ];
        while ( low <= high ) {
            mid = ( low + high ) >> 1;
            rk = getCountBelow( le, ri, data[ 0 ][ mid ] ) + 1;
            if ( rk <= k ) {
                low = mid + 1;
                if ( mid > ans ) {
                    ans = mid;
                }
            }
            else {
                high = mid - 1;
            }
        }
        return data[ 0 ][ ans ];
    }
private :
    T  data[ NL ][ 1 << NL ];
    int left[ 4 << NL ], right[ 4 << NL ];
};

// MergeTree end
