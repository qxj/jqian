template<class T>
void KMPinit( const T * pat, int patLen, int * flink ){
    int j, k;
    flink[ 0 ] = -1;
    for( j = 1; j < patLen; ++j ){
        k = flink[ j - 1 ];
        while( ( k != -1 ) && ( pat[ j - 1 ] != pat[ k ] ) ){
            k = flink[ k ];
        }
        flink[ j ] = k + 1;
    }
}
template<class T>
int KMPmatch( const T * txt, int txtLen, const T * pat, int patLen, const int * flink, int matBegin = 0 ){
    int i = matBegin, j = 0;
    while( ( i < txtLen ) && ( j < patLen ) ){
        while( ( j != -1 ) && ( txt[ i ] != pat[ j ] ) ){
            j = flink[ j ];
        }
        ++j;
        ++i;
    }
    return ( j >= patLen ? i - patLen : -1 );
}
