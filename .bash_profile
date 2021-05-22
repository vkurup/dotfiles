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

# Kevel
export PATH=$PATH:~/src/adzerk/teammgmt/bin/:~/src/adzerk/teammgmt/infrastructure/bin/:~/src/adzerk/cli-tools/micha
adzerk_env() {
  eval "$(gpg -d ~/.adzerk/env.asc)"
}
if [[ "$OSTYPE" == "darwin"* ]]; then
  # Mac OSX
  source "$HOME/.bashrc"
fi
