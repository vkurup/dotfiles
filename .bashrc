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

# some more ls aliases
#alias ll='ls -l'
#alias la='ls -A'
#alias l='ls -CF'

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
PS1="\u@\h:\w\$(parse_git_branch) $ "

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# add Dropbox/bin to path
if [ -d "$HOME/Dropbox/bin" ] ; then
    PATH="$HOME/Dropbox/bin:$PATH"
fi

# add Google appengine to path
if [ -d "$HOME/Dropbox/dev/google_appengine" ] ; then
    PATH="$HOME/Dropbox/dev/google_appengine:$PATH"
fi

# add android sdk to path
if [ -d "$HOME/opt/android-sdk-linux/tools" ] ; then
    PATH="$HOME/opt/android-sdk-linux/platform-tools:$HOME/opt/android-sdk-linux/tools:$PATH"
fi

# add cope
if [ -d "/usr/share/perl5/vendor_perl/auto/share/dist/Cope" ] ; then
    PATH="/usr/share/perl5/vendor_perl/auto/share/dist/Cope:$PATH"
fi

TIME_STYLE=long-iso

export LEDGER_FILE=${HOME}/Dropbox/ledger.dat
export EDITOR=emacsclient
export VISUAL=$EDITOR
export ALTERNATE_EDITOR=emacs
export CVS_RSH=ssh
export PAGER=most

export WORKON_HOME=$HOME/.virtualenvs
export PROJECT_HOME=$HOME/dev
source /usr/bin/virtualenvwrapper.sh
