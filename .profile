# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
umask 077

# set PATH so it includes user's private bin if it exists
export PATH="$HOME/.local/bin${PATH:+:$PATH}"

if [ -x /usr/bin/emacsclient ]; then
    export EDITOR='/usr/bin/emacsclient'
elif [ -x /usr/bin/emacs ]; then
    export EDITOR='/usr/bin/emacs'
elif [ -x /usr/bin/vim ]; then
    export EDITOR='/usr/bin/vim'
elif [ -x /usr/bin/nano ]; then
    export EDITOR='/usr/bin/nano'
fi

export SUDO_EDITOR="$EDITOR"
export VISUAL="$EDITOR"

eval `ssh-agent`

# ghcup
if [[ -d ~/.ghcup ]]; then
  export PATH=$HOME/.ghcup/bin:$PATH
fi

# opam
if type opam &>/dev/null; then
  if [[ -r ~/.local/share/opam/opam-init/init.zsh ]]; then
    source ~/.local/share/opam/opam-init/init.zsh &>/dev/null
  fi
fi

if [ -z "$XDG_CONFIG_HOME" ]; then
    export XDG_CONFIG_HOME=$HOME/.config
    export XDG_CACHE_HOME=$HOME/.cache
    export XDG_DATA_HOME=$HOME/.local/share
    export XDG_STATE_HOME=$HOME/.local/state
fi

if [ -x /opt/homebrew/bin/brew ]; then
  eval "$(/opt/homebrew/bin/brew shellenv)"
fi
