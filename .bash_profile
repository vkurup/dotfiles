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

if [[ "$OSTYPE" == "darwin"* ]]; then
  # Mac OSX
  source .bashrc
fi
