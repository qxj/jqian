#!/bin/sh
# linkdir.sh --- Time-stamp: <Julian Qian 2010-12-15 16:57:02>
# Copyright 2010 Julian Qian
# Author: jqian@julian-desktop
# Version: $Id: linkdir.sh,v 0.0 2010/12/15 08:34:59 jqian Exp $

# set -x


SDIR="$1"
TDIR="$2"

if [ ! -d $SDIR -a ! -d $TDIR ];
then
    echo "usage: linkdir <source-directory> <target-directory>"
    exit -1
fi

linkdir() {
    local dir=${1%/}
    local temp=${dir#$SDIR}
    local tdir=$TDIR/$temp
    local tdir=${tdir%/}
    if [ -d $dir -a $dir != "." -a $dir != ".." ];
    then
        echo "enter directory $dir"
        for item in `ls $dir`
        do
            if [ ! -d $dir/$item -a -f $dir/$item ];
            then
                ln -s $dir/$item $tdir/$item
                echo "link file $sdir/$item to $tdir/$item"
            elif [ -d $dir/$item ];
            then
                mkdir $tdir/$item
                echo "mkdir $tdir/$item"
                linkdir $dir/$item
            fi
        done
    else
        return 0
    fi
}

mkdir $TDIR
echo "mkdir top target $TDIR"

linkdir $SDIR

echo "linkdir doen!"