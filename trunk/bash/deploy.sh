#!/bin/sh

ipfile=$1
user=$2
passwd=$3

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

_ssh_login() {
    user=$1
    passwd=$2
    ip=$3
    expect -c "set timeout 30
spawn ssh $user@$ip -q
expect {
    \"assword:\" {
        send \"$passwd\r\"
        expect -re {[>$]} { send \"export LC_ALL=en_US.UTF-8\r\"
                            send \"export LC_CTYPE=zh_CN.UTF-8\r\" }
        interact
    }
    \"yes/no)?\" {
        send \"yes\r\"
        expect \"assword:\" {
            send \"$passwd\r\"
            expect -re {[>$]} { send \"export LC_ALL=en_US.UTF_8\r\"
                                send \"export LC_CTYPE=zh_CN.UTF-8\r\" }
            interact
        }
    }
    \"warning*\" {
        puts \"\nRETURN WARNING!!!\n\"
        exit 1
    }
    timeout {
        puts \"\nCHECK WARNING: $ip logon TIMEOUT!!!\n\"
        exit 1
    }
}
"
}

_ssh_run() {
    passwd=$1
    cmd=$2
    expect -c "set timeout 3600
spawn $cmd
expect {
    \"assword:\" {
        send \"$passwd\r\"
    }
    \"yes/no)?\" {
        send \"yes\r\"
        expect \"assword:\" {
            send \"$passwd\r\"
        }
    }
    \"warning*\" {
        puts \"\nRETURN WARNING!!!\n\"
        exit 1
    }
    timeout {
        puts \"\nCHECK WARNING: $ip logon TIMEOUT!!!\n\"
        exit 1
    }
}
expect eof
"
}

_ssh_exec() {
    user=$1
    passwd=$2
    ip=$3
    cmd=$4
    _ssh_run $passwd "ssh $user@$ip \"$cmd\""
}

_ssh_to() {
    user=$1
    passwd=$2
    ip=$3
    fa=$4
    fb=$5
    _ssh_run $passwd "scp -r $fa $user@$ip:$fb"
}

_ssh_from() {
    user=$1
    passwd=$2
    ip=$3
    fa=$4
    fb=$5
    _ssh_run $passwd "scp -r $user@$ip:$fa $fb"
}

cmdlogin() {
    ret=$(_hostinfo_search $1)
    user=$(_hostinfo_user "$ret")
    passwd=$(_hostinfo_passwd "$ret")
    ip=$(_hostinfo_ip "$ret")
    _ssh_login $user "$passwd" $ip
}

cmdexec() {
    ret=$(_hostinfo_search $1)
    user=$(_hostinfo_user "$ret")
    passwd=$(_hostinfo_passwd "$ret")
    ip=$(_hostinfo_ip "$ret")

    cmd=$2
    _ssh_exec $user "$passwd" $ip "$cmd"
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

    _ssh_to $user "$passwd" $ip $fa $fb
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

    _ssh_from $user "$passwd" $ip $fa $fb
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

        _ssh_exec $user "$passwd" $ip "$cmd"
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

        _ssh_to $user "$passwd" $ip $fa $fb
    done
    IFS=$bak_IFS
}

cmdlist () {
    echo "List "$HOSTINFO" content below:
----
NAME IP USERNAME PASSWORD DEFAULT-PATH
----"
    cat $HOSTINFO
    echo "
----"
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