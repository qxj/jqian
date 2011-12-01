#!/bin/sh
# connect.sh --- Time-stamp: < 2011-11-30 14:17:15>
# Copyright 2011
# Author: junist@gmail.com
# Version: $Id: connect.sh,v 0.0 2011/11/30 06:16:42 jqian Exp $

# set -x

# $HOME/.hostinfo.txt format:
#     host-alias host-ip login-user login-passwd
# each line is seperated by \n, end file with \n

HOST=$1

if [ -z $HOST ]
then
    echo "please specify a host name"
    exit 1
elif [ $HOST == "-h" -o $HOST == "--help" ]
then
    echo "Usage:
    $0 <hostname>"
    exit 0
fi

connect(){
    host=$1
    user=$2
    passwd=$3
    expect -c "spawn ssh -l $user $host
set timeout 30
expect \"*?assword*\"
send \"$passwd\r\"
interact"
}

{
while read line;  do
    req=$(echo $line | awk '{print $1}')
    if [[ "$req" == $HOST* ]]
    then
    echo "match request: $req"
    host=$(echo $line | awk '{print $2}')
    user=$(echo $line | awk '{print $3}')
    passwd=$(echo $line | awk '{print $4}')
    break
    fi
done
} < $HOME/.hostinfo.txt

if [ -n "$host" ]
then
    connect $host $user $passwd
else
    echo "no matched host"
fi

exit 0