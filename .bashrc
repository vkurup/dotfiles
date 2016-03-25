# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
# vk old version -> [ -z "$PS1" ] && return
case $- in
    *i*) ;;
      *) return;;
esac

export SSH_ASKPASS="/usr/bin/ssh-askpass"
eval `keychain --agents gpg,ssh --eval id_rsa 66832BC1`

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoreboth

export HISTFILESIZE=1000000000
export HISTSIZE=1000000

# check the window size after each command and, if necessary,
# update the values of LINES and COLUMNS.
shopt -s checkwinsize histappend

# Alias definitions.
if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

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

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
PS1="\u@\h:\w\$(parse_git_branch) $ "

TIME_STYLE=long-iso

export LEDGER_FILE=${HOME}/Dropbox/data/finance/ledger.dat
export EDITOR=emacsclient
export VISUAL=$EDITOR
export ALTERNATE_EDITOR=emacs
export CVS_RSH=ssh
export PAGER=most

export WORKON_HOME=$HOME/.virtualenvs
export VIRTUALENVWRAPPER_PYTHON=`which python`
export PROJECT_HOME=$HOME/dev
#source /usr/bin/virtualenvwrapper.sh

### Added by the Heroku Toolbelt
if [ -d /usr/local/heroku ]; then
    export PATH="/usr/local/heroku/bin:$PATH"
fi

# added by travis gem
[ -f /home/vkurup/.travis/travis.sh ] && source /home/vkurup/.travis/travis.sh

# install ansible from source
if [ -d "$HOME/dev/ansible" ] ; then
    source $HOME/dev/ansible/hacking/env-setup -q
fi
