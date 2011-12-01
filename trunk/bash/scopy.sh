#!/bin/sh
# scopy.sh --- Time-stamp: < 2011-11-30 14:18:14>
# Copyright 2011
# Author: junist@gmail.com
# Version: $Id: scopy.sh,v 0.0 2011/11/30 06:17:26 jqian Exp $

# set -x

HOW=$1              # to/from
HOST=$2
FA=$3
FB=$4

if [ -z $HOW -o $HOW == "-h" -o $HOW == "--help" -o -z $HOST -o -z $FA ]
then
    echo "Usage:
    $0 -to/-from <hostname> <source-path> <target-path>
E.g.:
    1. scp local ~/local.txt to remote ~/remote.txt under home dir:
      $0 to 10.1.123.12 ~/local.txt ~/remote.txt
    2. scp remote ~/src.t to local ~/local.txt:
      $0 from 10.1.123.12 ~/remote.txt ~/local.txt
    3. scp local ~/a.txt to remote ~/a.txt under home dir:
      $0 to 10.1.123.12 ~/a.txt
    4. scp remote ~/b.txt to local ~/b.txt:
      $0 f 10.1.123.12 ~/b.txt"
    exit 0
fi

sshcopy(){
    act=$1
    passwd=$2
    expect -c "spawn $act
set timeout 30
expect \"*?assword*\"
send \"$passwd\r\"
expect eof"
}

{
while read line;  do
    req=$(echo $line | awk '{print $1}')
    if [ "$req" = "$HOST" ]
    then
    echo "match request: $req"
    host=$(echo $line | awk '{print $2}')
    user=$(echo $line | awk '{print $3}')
    passwd=$(echo $line | awk '{print $4}')
    if [[ "$HOW" == f* ]]
    then
        # TODO: path include whitespace
        fa=${FA/~\//home\/$user\/}
        if [ -z $FB ]
        then
        fb="."
        else
        fb=$FB
        fi
        act="scp -r $user@$host:$fa $fb"
    else
        fa=$FA
        if [ -z $FB ]
        then
        fb="/home/$user/"
        else
        fb=${FB/~\//home\/$user\/}
        fi
        act="scp -r $fa $user@$host:$fb"
    fi
    break
    fi
done
} < $HOME/.hostinfo.txt

if [ -n "$act" ]
then
    sshcopy "$act" $passwd
else
    echo "no matched host"
fi

exit 0