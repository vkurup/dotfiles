#
# ~/.bash_profile
#

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# add Sync/Vinod/bin to path
if [ -d "$HOME/Sync/Vinod/bin" ] ; then
    PATH="$HOME/Sync/Vinod/bin:$PATH"
fi

# rbenv
if [ -d "$HOME/.rbenv" ]; then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
fi

# pyenv
if [ -d "$HOME/.pyenv" ]; then
  export PATH="$HOME/.pyenv/bin:$PATH"
  eval "$(pyenv init -)"
  eval "$(pyenv virtualenv-init -)"
  export PYENV_VIRTUALENV_DISABLE_PROMPT=1
fi

# brew psql 15
export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"

# Can't do this because our scripts expect a Mac sed if OS is Darwin
# gnu-sed instead of sed (from `brew info gnu-sed`)
#export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"

# Kevel
export PATH=$PATH:~/dev/teammgmt/bin:~/dev/teammgmt/infrastructure/bin:~/dev/cli-tools/micha:~/dev/cli-tools/scripts

source "$HOME/.bashrc"

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"
