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
    local last_cmd=$?
    #if [ -x /usr/bin/tput ] && tput setaf 1 >&/dev/null; then
    if true; then
        # Reset
        Color_Off='\e[0m'       # Text Reset

        # Regular Colors
        Black='\e[0;30m'        # Black
        Red='\e[0;31m'          # Red
        Green='\e[0;32m'        # Green
        Yellow='\e[0;33m'       # Yellow
        Blue='\e[0;34m'         # Blue
        Purple='\e[0;35m'       # Purple
        Cyan='\e[0;36m'         # Cyan
        White='\e[0;37m'        # White

        # Bold
        BBlack='\e[1;30m'       # Black
        BRed='\e[1;31m'         # Red
        BGreen='\e[1;32m'       # Green
        BYellow='\e[1;33m'      # Yellow
        BBlue='\e[1;34m'        # Blue
        BPurple='\e[1;35m'      # Purple
        BCyan='\e[1;36m'        # Cyan
        BWhite='\e[1;37m'       # White

        # Underline
        UBlack='\e[4;30m'       # Black
        URed='\e[4;31m'         # Red
        UGreen='\e[4;32m'       # Green
        UYellow='\e[4;33m'      # Yellow
        UBlue='\e[4;34m'        # Blue
        UPurple='\e[4;35m'      # Purple
        UCyan='\e[4;36m'        # Cyan
        UWhite='\e[4;37m'       # White

        # Background
        On_Black='\e[40m'       # Black
        On_Red='\e[41m'         # Red
        On_Green='\e[42m'       # Green
        On_Yellow='\e[43m'      # Yellow
        On_Blue='\e[44m'        # Blue
        On_Purple='\e[45m'      # Purple
        On_Cyan='\e[46m'        # Cyan
        On_White='\e[47m'       # White
    fi

    local p_user=$Red"\u"$Color_Off
    local p_host=$Green"\h"$Color_Off
    local p_path=$Blue"\w"$Color_Off
    local p_time=$Yellow"\t"$Color_Off
    local p_flag="$"

    # local p_last=$Green'\342\234\223'$Color_Off
    local p_last=
    if [ $last_cmd -ne 0 ]; then
        p_last=$On_Red'\342\234\227'$Color_Off
    fi
    if [ $(/usr/bin/id -u) -eq 0 ]; then
        p_user=""
        p_host=$Red"\h"$Color_Off
        p_flag="#"
    fi
    if [ -n "$SSH_CLIENT" ]; then
        p_host=$On_Green"\h"$Color_Off
    fi
    local p_git=
    if [ $(which git 2>/dev/null) ]; then
        p_git=$Purple'`git branch 2> /dev/null | grep -e ^* | sed -E  s/^\\\\\*\ \(.+\)$/\(\\\\\1\)\ /`'$Color_Off
    fi
    PS1=$p_last$p_user"@"$p_host"["$p_time"]:"$p_git$p_path"\n"$p_flag" "
}
PROMPT_COMMAND=my_prompt

# Keep the same history in all sessions
# PROMPT_COMMAND="${PROMPT_COMMAND:+$PROMPT_COMMAND ;} history -a; history -c; history -r;"

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

function colors() {
	local fgc bgc vals seq0

	printf "Color escapes are %s\n" '\e[${value};...;${value}m'
	printf "Values 30..37 are \e[33mforeground colors\e[m\n"
	printf "Values 40..47 are \e[43mbackground colors\e[m\n"
	printf "Value  1 gives a  \e[1mbold-faced look\e[m\n\n"

	# foreground colors
	for fgc in {30..37}; do
		# background colors
		for bgc in {40..47}; do
			fgc=${fgc#37} # white
			bgc=${bgc#40} # black

			vals="${fgc:+$fgc;}${bgc}"
			vals=${vals%%;}

			seq0="${vals:+\e[${vals}m}"
			printf "  %-9s" "${seq0:-(default)}"
			printf " ${seq0}TEXT\e[m"
			printf " \e[${vals:+${vals+$vals;}}1mBOLD\e[m"
		done
		echo; echo
	done
}

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
case $OSTYPE in
    darwin*)
        test -f $(brew --prefix)/etc/bash_completion && . $(brew --prefix)/etc/bash_completion
        ;;
    linux*)
        test -f /etc/bash_completion && . /etc/bash_completion
        test -f /usr/share/autojump/autojump.sh && . /usr/share/autojump/autojump.sh
        ;;
esac

# Env variables
test -d $HOME/bin && export PATH=$PATH:$HOME/bin
test -d $HOME/local/bin && export PATH=$PATH:$HOME/local/bin
test -d $HOME/.local/bin && export PATH=$PATH:$HOME/.local/bin
test -d $HOME/.cask/bin && export PATH=$PATH:$HOME/.cask/bin
export PAGER=less
export LESS="-XR"               # interpret "raw" control sequences (ipython)
export EDITOR=vim

# Mapping Caps Lock to Control
# setxkbmap -layout us -option ctrl:nocaps

test -f ~/.bash_alias && . ~/.bash_alias
test -f ~/.bash_local && . ~/.bash_local
