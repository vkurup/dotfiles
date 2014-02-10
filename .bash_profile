#
# ~/.bash_profile
#

eval `keychain --clear --eval id_rsa 66832BC1`

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

# jmeter
export PATH="/usr/local/apache-jmeter-2.11/bin:$PATH"

[[ -z $DISPLAY && $XDG_VTNR -eq 1 ]] && startx
