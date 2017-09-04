# If not running interactively, stop now
case $- in
    *i*) ;;
      *) return;;
esac

[ -f /etc/bashrc ] && . /etc/bashrc
[ -f ~/.shrc ] && . ~/.shrc
[ -f ~/.bashrc_local ] && . ~/.bashrc_local

# Don't put duplicate lines or lines starting with space in the history.
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it
shopt -s histappend

# For setting history length see HISTSIZE and HISTFILESIZE in bash(1)
HISTSIZE=1000
HISTFILESIZE=10000
PROMPT_COMMAND=_prompt_command
_prompt_command() {
    #https://github.com/demure/dotfiles/blob/master/subbash/prompt

    local ret_code="$?"
    history -a
    history -n

    local reset='\[\e[0m\]'
    local red='\[\e[0;31m\]'
    local green='\[\e[0;32m\]'
    local light_green='\[\e[1;32m\]'
    local yellow='\[\e[0;33m\]'
    local blue='\[\e[0;34m\]'

    PS1="$reset"
    if [ $ret_code != 0 ]; then
        PS1+="$red($ret_code)$reset "
    fi

    if [ -n "${VIRTUAL_ENV}" ]; then
        PS1+="$reset(venv) "
    fi

    PS1+="${light_green}\u@\h${reset} "

    # if [ jobs >/dev/null 2>&1 ]; then
    #     local bgjobs=$(jobs -r | wc -l | tr -d " ")
    #     local stoppedjobs=$(jobs -s | wc -l | tr -d " ")
    #     if [ "$bgjobs" -gt 0 ] || [ "$stoppedjobs" -gt 0 ]; then
    #         PS1+="${reset}["
    #         if [ $bgjobs -gt 0 ]; then
    #             PS1+="${reset}${bgjobs} bg"
    #             [ $stoppedjobs -gt 0 ] && PS1+=", "
    #         fi
    #         [ $stoppedjobs -gt 0 ] && PS1+="${reset}${stoppedjobs} stopped"
    #         PS1+="]${reset} "
    #     fi
    # fi

    local tilde='~'
    PS1+="${blue}${PWD/#$HOME/$tilde} ${reset}\$ "
}

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize

# If set, the pattern "**" used in a pathname expansion context will
# match all files and zero or more directories and subdirectories.
shopt -s globstar

# enable programmable completion features (you don't need to enable
# this, if it's already enabled in /etc/bash.bashrc and /etc/profile
# sources /etc/bash.bashrc).
if ! shopt -oq posix; then
  if [ -f /usr/share/bash-completion/bash_completion ]; then
    . /usr/share/bash-completion/bash_completion
  elif [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
  fi
fi
