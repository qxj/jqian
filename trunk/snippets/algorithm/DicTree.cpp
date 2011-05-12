// DictInsert( &rootDict, s, id )
// DictSearch( rootDict, s )
#include <stdlib.h>
#define  T  250
typedef struct __DICT
{
    int id;
    struct __DICT * ch[T];
} DICT;
DICT * rootDict = 0;
void DictInsert( DICT ** roo, const char * p, int id ){
    for(;;){
        if( *roo == 0 ){
            *roo = (DICT*)malloc( sizeof(DICT) );
            (*roo)->id = 0;
            memset( (*roo)->ch, 0, sizeof( (*roo)->ch ) );
        }
        if( *p ){
            roo = &( (*roo)->ch[*p++] );
        }
        else{
            (*roo)->id = id;
            return;
        }
    }
}
int DictSearch( DICT * roo, const char * p ){
    for(;;){
        if( roo == 0 ){
            return 0;
        }
        if( *p ){
            roo = roo->ch[*p++];
        }
        else{
            return roo->id;
        }
    }
}
