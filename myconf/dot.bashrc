# -*- mode: sh -*-
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history.
HISTCONTROL=ignoredups:ignorespace:erasedups
HISTSIZE=10000
HISTFILESIZE=20000

# Make some commands not show up in history
HISTIGNORE="?:??:cd -:top:pwd:exit:date:* --help"

# Keep the same history in all sessions
PROMPT_COMMAND="history -a; history -c; history -r; $PROMPT_COMMAND"

# Don’t clear the screen after quitting a manual page
MANPAGER="less -X"

# Don't grep svn base
GREP_OPTIONS="--color --exclude=\*.svn-base"

# append to the history file, don't overwrite it
shopt -s histappend

# Case-insensitive globbing (used in pathname expansion)
shopt -s nocaseglob

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# Autocorrect typos in path names when using `cd`
shopt -s cdspell

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# make prompt more friendly
function my_prompt()
{
    if [ -z "$debian_chroot" ] && [ -r /etc/debian_chroot ]; then
        debian_chroot=$(cat /etc/debian_chroot)
    fi
    local is_color=
    if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
        is_color=yes
    fi
    local is_root=
    if [ $(/usr/bin/id -u) -eq 0 ]; then
        is_root=yes
    fi
    local git_branch=
    if [ $(which git) ]; then
        git_branch='`git branch 2> /dev/null | grep -e ^* | sed -E  s/^\\\\\*\ \(.+\)$/\(\\\\\1\)\ /`'
    fi

    if [ $is_color ]; then
        if [ $is_root ]; then
            PS1="${debian_chroot:+($debian_chroot)}\[\033[01;31m\]\h\[\033[00m\][\[\033[01;33m\]\t\[\033[00m\]]:\[\033[01;34m\]\w\[\033[00m\]\n# "
        else
            PS1="${debian_chroot:+($debian_chroot)}\[\033[01;31m\]\u\[\033[00m\]@\[\033[01;32m\]\h\[\033[00m\][\[\033[01;33m\]\t\[\033[00m\]]:\[\033[01;35m\]$git_branch\[\033[00m\]\[\033[01;34m\]\w\[\033[00m\]\n$ "
        fi
    else
        if [ $is_root ]; then
            PS1="${debian_chroot:+($debian_chroot)}\h[\t]:\w\n# "
        else
            PS1="${debian_chroot:+($debian_chroot)}\u@\h[\t]:\w\n$ "
        fi
    fi
}
my_prompt

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

alias urldecode="python -c \"import re,sys;print re.sub(r'%([0-9a-hA-H]{2})',lambda m: chr(int(m.group(1),16)), open(sys.argv[1]).read() if len(sys.argv) > 1 else sys.stdin.read())\""

# timestamp to date string
# e.g.: ts2ds "2013-01-01 12:01:05" "+%Y-%m-%d %H:%M:%S"
function ts2ds()
{
    if [[ -n $2 ]]; then
        fmt=$2
    else
        fmt="+%Y%m%d"
    fi
    date -d "1970-01-01 utc $1 seconds" $fmt
}

# date string to timestamp
function ds2ts()
{
    date -d "$1" +%s
}

# print date string one-by-one from one day to another day
# e.g.: print_ds 20130103 20130604 +%Y%m%d
function print_ds()
{
    if [[ -n $1 ]]; then
        ds1=today
    else
        ds1=$1
    fi
    if [[ -n $2 ]]; then
        ds2=today
    else
        ds2=$2
    fi
    if [[ -n $3 ]]; then
        fmt=$3
    else
        fmt="+%Y%m%d"
    fi
    ts1=$(ds2ts $1)
    ts2=$(ds2ts $2)
    if [[ $ts1 -gt $ts2 ]]; then
        ((periods=($ts1-$ts2)/86400))
        incr=0
    else
        ((periods=($ts2-$ts1)/86400))
        incr=1
    fi
    for((i=0;i<=periods;i++));
    do
        if [[ $incr -eq 1 ]]; then
            ((ts=$ts1+86400*$i))
        else
            ((ts=$ts1-86400*$i))
        fi
        echo $(ts2ds $ts $fmt)
    done
}

function settitle()
{
  echo -ne "\e]2;$@\a\e]1;$@\a";
}

# This function defines a 'cd' replacement function capable of keeping,
# displaying and accessing history of visited directories, up to 10 entries.
# To use it, uncomment it, source this file and try 'cd --'.
# acd_func 1.0.5, 10-nov-2004
# Petar Marinov, http:/geocities.com/h2428, this is public domain

function cd_func()
{
  local x2 the_new_dir adir index
  local -i cnt

  if [[ $1 ==  "--" ]]; then
    dirs -v
    return 0
  fi

  the_new_dir=$1
  [[ -z $1 ]] && the_new_dir=$HOME

  if [[ ${the_new_dir:0:1} == '-' ]]; then
    #
    # Extract dir N from dirs
    index=${the_new_dir:1}
    [[ -z $index ]] && index=1
    adir=$(dirs +$index)
    [[ -z $adir ]] && return 1
    the_new_dir=$adir
  fi

  #
  # '~' has to be substituted by ${HOME}
  [[ ${the_new_dir:0:1} == '~' ]] && the_new_dir="${HOME}${the_new_dir:1}"

  #
  # Now change to the new dir and add to the top of the stack
  pushd "${the_new_dir}" > /dev/null
  [[ $? -ne 0 ]] && return 1
  the_new_dir=$(pwd)

  #
  # Trim down everything beyond 11th entry
  popd -n +11 2>/dev/null 1>/dev/null

  #
  # Remove any other occurence of this dir, skipping the top of the stack
  for ((cnt=1; cnt <= 10; cnt++)); do
    x2=$(dirs +${cnt} 2>/dev/null)
    [[ $? -ne 0 ]] && return 0
    [[ ${x2:0:1} == '~' ]] && x2="${HOME}${x2:1}"
    if [[ "${x2}" == "${the_new_dir}" ]]; then
      popd -n +$cnt 2>/dev/null 1>/dev/null
      cnt=cnt-1
    fi
  done

  return 0
}
alias cd=cd_func

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
case $OSTYPE in
    darwin*)
        test -f $(brew --prefix)/etc/bash_completion && . $(brew --prefix)/etc/bash_completion
        ;;
    linux)
        test -f /etc/bash_completion && . /etc/bash_completion
        ;;
esac

# Env variables
test -d $HOME/bin && export PATH=$PATH:$HOME/bin

test -f ~/.bash_aliases && . ~/.bash_aliases
test -f ~/.bash_local && . ~/.bash_local
