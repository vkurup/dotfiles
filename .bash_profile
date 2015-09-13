#
# ~/.bash_profile
#

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

# add node.js
if [ -d "$HOME/node_modules/.bin" ] ; then
    PATH="$HOME/node_modules/.bin:$PATH"
fi

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

# install ansible from source
if [ -d "$HOME/dev/ansible" ] ; then
    source $HOME/dev/ansible/hacking/env-setup
fi
