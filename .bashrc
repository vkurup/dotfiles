# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
[ -z "$PS1" ] && return

# don't put duplicate lines in the history. See bash(1) for more options
export HISTCONTROL=ignoredups
# ... and ignore same sucessive entries.
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
if [ -f /etc/bash_completion ] && ! shopt -oq posix; then
    . /etc/bash_completion
fi

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
PS1="\u@\h:\w\$(parse_git_branch) $ "

TIME_STYLE=long-iso

keychain id_rsa
. ~/.keychain/`uname -n`-sh

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

# jmeter
export PATH="${PATH}:/usr/local/apache-jmeter-2.9/bin"
