#!/bin/sh
# @(#) common.sh  Time-stamp: <Julian Qian 2013-04-08 23:51:52>
# Copyright 2012, 2013
# Author:  <junist@gmail.com>
# Version: $Id: common.sh,v 0.1 2012-03-24 11:26:17 jqian Exp $
#

export PATH=/home/hadoop/hadoop/bin:$PATH

################
# helper functions
################
die() {
    # @arg dying message
    #
    echo "[1;31mERROR[0m: $1. Aborting!"
    exit 1
}

single_instance() {
    # @arg locking file name, optional
    #
    local LOCKFILE=$(dirname $0).lck
    if [[ -n $1 ]]; then
        LOCKFILE=$1
    fi
    [ -f $LOCKFILE ] && die "[1;34mAnother INSTANCE ($LOCKFILE) is running?[0m"
    trap "{ rm -f $LOCKFILE ; exit 0; }" EXIT SIGTERM SIGINT
    touch $LOCKFILE
}

info() {
    # @arg info msg
    #
    local msg=$1
    toagent=$(find /usr/local/tips_agent* -name 'toagent')
    $toagent 32 "mail" "jqian" "[INFO] $msg" ""
}

error() {
    # @arg error msg
    #
    local msg=$1
    toagent=$(find /usr/local/tips_agent* -name 'toagent')
    $toagent 32 "mail|sms|rtx" "jqian" "[ERROR] $msg" ""
}

################
# timestamp
################
ts2str() {
    date -d "1970-01-01 utc $1 seconds" "+%Y-%m-%d %H:%M:%S"
}

str2ts() {
    date -d "$1" +%s
}

ts2ds() {
    date -d "1970-01-01 utc $1 seconds" "+%Y%m%d"
}

ds2ts() {
    date -d "$1" +%s
}

################
# ssh & scp
#
# USAGE:
#
# ssh_exec wbsvr password 10.177.153.150 "cd $HOME/jqian/guess-you-like && gzip -d result.gz"
# ssh_to wbsvr password 10.177.153.150 $stat_file /data/wbsvr/jqian/wpd-apply-helper
################
_expect_passwd() {
    local passwd=$1 cmd=$2
    expect -c "set timeout 3600                                                 
log_user 0                                                                      
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
        puts \"\nCHECK WARNING: logon TIMEOUT!!!\n\"                        
        exit 1                                                                  
    }                                                                           
}                                                                               
expect {                                                                        
    \"Authentication*\" {}                                                      
    \"Received*\" {}                                                            
}                                                                               
log_user 1                                                                      
expect eof                                                                      
"
}

ssh_exec() {
    local ip=$1 user=$2 passwd=$3 cmd=$4
    _expect_passwd $passwd "/usr/local/bin/ssh $user@$ip \"$cmd\""
}

ssh_to() {
    local ip=$1 user=$2 passwd=$3 source=$4 target=$5
    _expect_passwd $passwd "/usr/local/bin/scp -c blowfish -r $source $user@$ip:$target"
}

################
# clean data
################
clean_data() {
    # @arg path in hdfs
    # @arg how many recent directories to clean
    #
    local path=$1 num=$2                      # reserved recent three copy
    if [[ -z $num ]]; then
        num=3
    fi
    input_list=( $(hadoop fs -dus $path/ds=* 2> /dev/null | awk '{i=$1; sub(/.*:[0-9]*\//,"/",i); print i;}') )
    input_len=${#input_list[*]}

    for((i=0; i<$((${input_len} - $num)); i++)); do
        echo ${input_list[$i]}
        hadoop fs -rmr ${input_list[$i]}
    done
}


################
# $ hadoop fs -dus /user/garyci/sns_write/ds=* | awk '{i=$1; sub(/.*:[0-9]*\//,"/",i); print i;}' | tail -1
# /user/garyci/sns_write/ds=20120320
# -> 20120320
################
get_tailing_date () {
    # @arg a string tailing with "ds=??????"
    # @echo tail data string, 8 charactors
    #
    local str=$1
    # echo ${str##*=}
    echo ${str:${#str}-8:8}
}

# check directory whether containing completed data
check_completed_dir () {
    # @arg path in hdfs
    # @arg flooring size, optional
    # @echo ok / failed
    #
    local dir=$1 floorSize=$2
    local is_success="no"
    hadoop fs -test -e $dir"/_SUCCESS" 2> /dev/null >/dev/null
    if [[ $? -eq 0 ]]; then
        is_success="yes"
    fi
    local is_completed="yes"
    if [[ -n $floorSize ]]; then
        is_completed="no"
        dirsize=$(hadoop fs -dus $dir 2> /dev/null | awk '{print $2}')
        # TODO: which size can prove the directory is not empty ?
        if ((dirsize>$floorSize)); then
            is_completed="yes"
        fi
    fi
    if [[ $is_success == "yes" && $is_completed == "yes" ]]; then
        echo "ok"
    else
        echo "failed"
    fi
}

################
# /user/jqian/wbfilter-logs/flaged_*.txt.gz
# -> /user/jqian/wbfilter-logs/flaged_20130327.txt.gz
################
latest_files () {
    local pattern=$1 num=$2
    if [[ -z $num ]]; then
        num=1
    fi
    local files=($(hadoop fs -ls $pattern 2>/dev/null | tail -$num | awk '{print $NF}'))
    local rets=
    for file in ${files[@]}; do
        rets=$file","$rets
    done
    echo ${rets%,}
}

################
# /user/jqian/micro_data_proj/0_active_sns/step1/ds=*
# -> /user/jqian/micro_data_proj/0_active_sns/step1/ds=20120320
#
# /user/jqian/micro_data_proj/0_active_sns/step1/ds=* 2
# -> /user/jqian/micro_data_proj/0_active_sns/step1/ds=20120319,/user/jqian/micro_data_proj/0_active_sns/step1/ds=20120320
################
latest_input () {
    # @arg input path in hdfs
    # @arg number to indicate how many recent path
    # @arg flooring Size, notest/ ?MB, optional
    # @echo a string containing lastest input pathes, seperated by comma
    #
    local input=$1
    lolca num=$2
    local pred=$3               # for no _SUCCESS directories, customize
                                # individual predication.
                                # specify how many MB
    local from_ds=$4            # from one date until `num`
    if [[ -z $num ]]; then
        num=1
    fi
    if [[ $pred == "notest" ]]; then
        pred=0
    fi
    local dirs=($(hadoop fs -dus $input 2> /dev/null | awk '{i=$1; sub(/.*:[0-9]*\//,"/",i); print i;}'))
    declare -a latest_dirs
    local j=0
    local matched_ds=
    local from_ts=
    if [[ -z $from_ds ]]; then
        matched_ds="ok"
    else
        from_ts=$(ds2ts $from_ds)
    fi
    for((i=${#dirs[*]}-1;i>=0;i--)); do
        local dir=${dirs[$i]}

        local filled=
        if [[ -n $pred ]]; then
            dirsize=$(hadoop fs -dus $dir 2> /dev/null | awk '{print $2}')
            if ((dirsize>pred*1000000)); then
                filled="ok"
            fi
        fi

        if [[ -z $matched_ds ]]; then
            day=$(get_tailing_date $dir)
            day_ts=$(ds2ts $day)
            if((day_ts<=from_ts)); then
                matched_ds="ok"
            fi
        fi
        if [[ $matched_ds == "ok" && ( $filled == "ok" || $(check_completed_dir $dir) == "ok" ) ]]; then
            latest_dirs[$j]=$dir
            ((j++))
            ((num--))
            if (($num<=0)); then
                break
            fi
        fi
    done
    if [[ $j -eq 0 ]]; then
        # error "No proper input result for $input, at $model"
        exit 1
    fi
    local ret=
    for dir in ${latest_dirs[*]}; do
        ret=$dir","$ret
    done
    echo ${ret%,}
}

# depend on $OVERWRITE_OUTPUT
latest_output () {
    # @arg output path in hdfs
    # @arg depended input path in hdfs, only by tailing date, optional
    # @echo output directory
    # @return succeed 0/ failed 1, 255
    #
    local dir=$1
    local input=$2               # output date depends on input's
    local output=$dir
    if [[ -n $input ]]; then
        date=$(get_tailing_date $input)
        if [[ -z $date ]]; then
            error "Missing latest input data: $input (output: $dir)"
            exit 255
        fi
        output="$dir/ds=$date"
    fi

    if [[ $(check_completed_dir $output) == "ok" ]]; then
        if [[ -n $OVERWRITE_OUTPUT ]]; then
            hadoop fs -rmr $output 2>/dev/null >/dev/null
        else
            info "output "$output" is already existed, at $output"
            echo $output
            return 1
        fi
    else
        hadoop fs -test -d $output 2> /dev/null >/dev/null
        if [[ $? -eq 0 ]]; then
            hadoop fs -rmr $output 2> /dev/null >/dev/null
        fi
    fi

    echo $output
    return 0
}

# Check whether output succeed to generate.
#
# Final output is ALWAYS named as "result", thus if "result" is
# generate, a flag named "_flag_completed" will be touched in directory.
#
check_output() {
    # @arg input path in hdfs
    #
    local dir=$1
    sleep 30                    # wait hdfs to flush
    hadoop fs -test -e $dir"/_SUCCESS" 2> /dev/null >/dev/null
    if [[ $? -eq 0 ]]; then
        info "SUCCEED to generate output $dir."
        isret=$(echo $dir | grep "result")
        if [[ -n $isret ]]; then
            rm -f _flag_*
            echo $(date) > _flag_completed
        fi
    else
        error "FAILED to generate output $dir!"
        rm -f _flag_*
        echo "$dir "$(date) >> _flag_failed
        exit 1
    fi
}

pre_job() {
    # @arg locking file name, optional
    #
    cd $(dirname $0)
    LOCKFILE=$(basename $(pwd)).lck
    if [[ -n $1 ]]; then
        LOCKFILE=$1
    fi
    [ -f $LOCKFILE ] && die "[1;34mAnother M/R JOB ($LOCKFILE) is running?[0m"
    trap "{ rm -f $LOCKFILE _flag_running ; exit 0; }" EXIT SIGTERM SIGINT
    touch $LOCKFILE
    rm -f _flag_* && echo $(date) > _flag_running
    # exit 0
}

# /user/jqian/micro_data_proj/0_active_sns/step1/ds=20120320
# -> /user/jqian/micro_data_proj/0_active_sns/step1/ds=20120320.processed
processed_input() {
    dir=$1
    hadoop fs -mv $dir "${dir}.processed"
}

debug() {
    echo "[1;31m[INPUT][0m: $input"
    echo "[1;31m[OUTPUT][0m: $output"
}

# OVERWRITE_OUTPUT=

################
# active user input from garyci
################
latest_user_input() {
    echo $(hadoop fs -dus /user/garyci/active_user/ds=* 2> /dev/null | awk '$2>600000000' | awk '{i=$1; sub(/.*:[0-9]*\//,"/",i); print i;}' | tail -1)
}

################################################################
## WARNING: `model`` variable depends on directories, must be referenced
## in the scripts: run_job.sh
##
workpath="/user/jqian/works"
model=$(pwd | awk -F/ '{print $NF}')
# date=$(date -d "2 days ago" +%Y%m%d)
yesterday=$(date -d "yesterday" +%Y%m%d)

#### test
# input=$(latest_input0 /user/carlkang/filter_topic_weight_feature_fawen)
# echo $input
# output=$(latest_output step1 $input)
# echo $output
