#!/bin/sh
# gbk2utf8.sh --- Time-stamp: < 2011-12-13 17:21:07>
# Copyright 2011
# Author: junist@gmail.com
# Version: $Id: gbk2utf8.sh,v 0.0 2011/12/02 06:39:54 jqian Exp $

# set -x

DIR=$1

gbk2utf8() {
    f=$1
    tmpf=/tmp/${f##*/}
    iconv -f gbk -t utf-8 $f > $tmpf && mv $tmpf $f
}

if [ -z $DIR ]
then
    echo "Usage:
    $0 <dir or file path>

Convert all c/cpp files encoding from gbk to utf8."
    exit 0
fi

if [ -d $DIR ]
then
    echo "converting $DIR"
    for f in $(find . -type f -iname "*.[hc]" -o -iname "*.cpp")
    do
        echo "converting $f"
        gbk2utf8 $f
    done
elif [ -f $DIR ]
then
    f=$DIR
    echo "converting $f"
    gbk2utf8 $f
fi
