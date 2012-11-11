#
# ~/.bash_profile
#

eval $(keychain --eval -Q --quiet id_rsa)

[[ -f ~/.bashrc ]] && . ~/.bashrc

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

# rbenv
export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"

startx
