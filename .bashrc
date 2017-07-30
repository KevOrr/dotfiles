# If not running interactively, stop now
case $- in
    *i*) ;;
      *) return;;
esac

[ -f ~/.setcolors ] && ~/.setcolors

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

    PS1+="${light_green}${USER}@`hostname`${reset} "

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
#shopt -s globstar

# make less more friendly for non-text input files, see lesspipe(1)
[ -x /usr/bin/lesspipe ] && eval "$(SHELL=/bin/sh lesspipe)"

# enable color support of ls and also add handy aliases
if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'
    #alias dir='dir --color=auto'
    #alias vdir='vdir --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

# colored GCC warnings and errors
export GCC_COLORS='error=01;31:warning=01;35:note=01;36:caret=01;32:locus=01:quote=01'

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

###########
# Aliases #
###########

[ -f ~/.gitaliases ] && . ~/.gitaliases

alias ll='ls -lF'
alias la='ls -A'
alias lal='ls -alF'
alias l='ls -CF'

alias o='xdg-open'
alias c=clear
alias clip='xclip -selection clipboard'

alias ec='emacsclient'
alias ecc='emacsclient -c'
alias ecnc='emacsclient -nc'
sudoec () { SUDO_EDITOR=emacsclient sudoedit $@; }
sudoecc () { SUDO_EDITOR='emacsclient -c' sudoedit $@; }
alias gdb='gdb --quiet'

setgov () { echo $1 | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor; }
getgov () { cat /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor; }

# http://www.bashoneliners.com/oneliners/oneliner/199/
alias randtext="tr -dc a-z1-5 </dev/urandom | tr 1-2 ' \n' | awk 'length==0 || length>50' | tr 3-5 ' ' | sed 's/^ *//' | cat -s | fmt"

randpass () { head -c$(( ($1+1)/2 )) </dev/random | xxd -p | head -c$1 | sed -e 'a\'; }
urandpass () { head -c$(( ($1+1)/2 )) </dev/urandom | xxd -p | head -c$1 | sed     -e 'a\'; }

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

ptrace_on () { echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope; }
ptrace_off () { echo 1 | sudo tee /proc/sys/kernel/yama/ptrace_scope; }

if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
    eval "$(pyenv virtualenv-init -)"
fi

if [ -d "/usr/local/cuda" ]; then
   export CUDA_HOME="/usr/local/cuda"
   if [[ -n $LD_LIBRARY_PATH ]]; then
       export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:$CUDA_HOME/lib64:$CUDA_HOME/extras/CUPTI/lib64"
   else
       export LD_LIBRARY_PATH="$CUDA_HOME/lib64:$CUDA_HOME/extras/CUPTI/lib64"
   fi
fi
