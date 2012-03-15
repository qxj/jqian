#!/bin/sh

ipfile=$1
user=$2
passwd=$3

EXPSCRIPT="ssh.exp"
if [ -z $HOSTINFO ]; then
    HOSTINFO=$HOME"/.hostinfo.txt"
fi

help() {
    cmd=$1
    echo "Usage:
$cmd <cmd> <parameters>

There are these cmd types:

1. login, login to a host automatically, eg:
$cmd login host-name

2. exec, execute some commands in a remote host, eg:
$cmd exec host-name \"free | tail -n 1 | awk '{print $3}'\"

3. to, copy local file/dir to a remote host, eg:
$cmd to host-name /local/path (/remote/path)

4. from, copy file/dir from a remote host to local, eg:
$cmd from host-name /remote/path (/local/path)

5. depexec, execute some commands in all remote host one by one, eg:
$cmd depexec \"free | tail -n 1 | awk '{print $3}'\"

6. depto, deploy local file to all remote hosts, eg:
$cmd depto /local/path (/remote/path)

----------------
$HOSTINFO format:
host-name host-ip user password default-path

eg:

dev 10.132.111.104 jqian password /data/jqian
"
    exit 1
}

_hostinfo_search() {
    bak_IFS=$IFS
    IFS="
"
    req=$1
    for line in $(cat $HOSTINFO); do
        host=$(echo $line | awk '{print $1}')
        if [[ $host == $req* ]]; then
            echo $line
            break
        fi
    done
    IFS=$bak_IFS
}

_hostinfo_ip() {
    echo $1 | awk '{print $2}'
}

_hostinfo_user() {
    echo $1 | awk '{print $3}'
}

_hostinfo_passwd() {
    echo $1 | awk '{print $4}'
}

_hostinfo_path() {
    echo $1 | awk '{print $5}'
}

cmdlogin() {
    ret=$(_hostinfo_search $1)
    user=$(_hostinfo_user "$ret")
    passwd=$(_hostinfo_passwd "$ret")
    ip=$(_hostinfo_ip "$ret")
    eval "$EXPSCRIPT login $user '$passwd' $ip"
}

cmdexec() {
    ret=$(_hostinfo_search $1)
    user=$(_hostinfo_user "$ret")
    passwd=$(_hostinfo_passwd "$ret")
    ip=$(_hostinfo_ip "$ret")

    cmd=$2
    eval "$EXPSCRIPT ssh $user '$passwd' $ip \"$cmd\" output.txt"
}

cmdto() {
    ret=$(_hostinfo_search $1)
    user=$(_hostinfo_user "$ret")
    passwd=$(_hostinfo_passwd "$ret")
    ip=$(_hostinfo_ip "$ret")
    path=$(_hostinfo_path "$ret")

    fa=$2
    fb=$3
    if [ -z $fb ]; then
        if [ -z $path ]; then
            fb="/home/$user/"
        else
            fb=$path
        fi
    else
        fb=${fb/~\//home\/$user\/}
    fi

    eval "$EXPSCRIPT scp $user '$passwd' $ip $fa $fb"
}

cmdfrom() {
    ret=$(_hostinfo_search $1)
    user=$(_hostinfo_user "$ret")
    passwd=$(_hostinfo_passwd "$ret")
    ip=$(_hostinfo_ip "$ret")
    path=$(_hostinfo_path "$ret")

    fa=$2
    fb=$3
    if [ -z $path ]; then
        fa=${fa/~\//home\/$user\/}
    else
        fa=$path/$fa
    fi
    if [ -z $fb ]; then
        fb="."
    else
        fb=$fb
    fi

    eval "$EXPSCRIPT scpfrom $user '$passwd' $ip '$fa' '$fb'"
}

cmddepexec () {
    bak_IFS=$IFS
    IFS="
"
    cmd=$1
    for line in $(cat $HOSTINFO); do
        user=$(_hostinfo_user $line)
        passwd=$(_hostinfo_passwd $line)
        ip=$(_hostinfo_ip $line)

        eval "$EXPSCRIPT ssh $user '$passwd' $ip \"$cmd\" output.txt"
    done
    IFS=$bak_IFS
}

cmddepto () {
    bak_IFS=$IFS
    IFS="
"
    fa=$1
    fb=$2
    for line in $(cat $HOSTINFO); do
        user=$(_hostinfo_user $line)
        passwd=$(_hostinfo_passwd $line)
        ip=$(_hostinfo_ip $line)
        path=$(_hostinfo_path $line)

        if [ -z $fb ]; then
            if [ -z $path ]; then
                fb="/home/$user/"
            else
                fb=$path
            fi
        else
            fb=${fb/~\//home\/$user\/}
        fi

        eval "$EXPSCRIPT scp $user '$passwd' $ip '$fa' '$fb'"
    done
    IFS=$bak_IFS
}

cmdlist () {
    echo "List "$HOSTINFO" content below:"
    cat $HOSTINFO
}

case $1X in
    loginX)
        cmdlogin $2
        ;;
    execX)
        cmdexec $2 $3
        ;;
    toX)
        cmdto $2 $3 $4
        ;;
    fromX)
        cmdfrom $2 $3 $4
        ;;
    depexecX)
        cmddepexec $2
        ;;
    deptoX)
        cmddepto $2 $3
        ;;
    listX)
        cmdlist
        ;;
    *)
        help $0
        ;;
esac

exit 0