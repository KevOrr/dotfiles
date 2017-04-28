# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

if [ -d "$HOME/.local/bin" ] ; then
    PATH="$PATH:$HOME/.local/bin"
fi

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
if [ -e /home/kevin/.nix-profile/etc/profile.d/nix.sh ]; then . /home/kevin/.nix-profile/etc/profile.d/nix.sh; fi # added by Nix installer

if [ -d "$HOME/.pyenv" ]; then
    export PYENV_ROOT="$HOME/.pyenv"
    export PATH="$PYENV_ROOT/bin:$PATH"
    eval "$(pyenv init -)"
fi
