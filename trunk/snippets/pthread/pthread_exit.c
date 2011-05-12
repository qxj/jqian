/* @(#)pthread_exit.c
 * Time-stamp: <Julian Qian 2011-05-09 20:57:38>
 * Copyright 2011 Julian Qian
 * Version: $Id: pthread_exit.c,v 0.0 2011/05/09 12:53:26 jqian Exp $
 */

#include <stdio.h>
#include <pthread.h>

int main(void) {
    pthread_t fd;
    void* a();
    char* b;
    pthread_create(&fd, NULL, a, NULL);
    pthread_join(fd, (void**)&b);

    printf("%s", b);
    return 0;
}

void* a() {
    char *p = "asdf";
    pthread_exit(p);
}
