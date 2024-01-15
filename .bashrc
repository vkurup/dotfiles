# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

# If not running interactively, don't do anything
case $- in
    *i*) ;;
      *) return;;
esac

black="\[\033[0;30m\]"
red="\[\033[0;31m\]"
green="\[\033[0;32m\]"
yellow="\[\033[0;33m\]"
blue="\[\033[0;34m\]"
purple="\[\033[0;35m\]"
cyan="\[\033[0;36m\]"
white="\[\033[0;37m\]"

if [[ "$OSTYPE" == "linux-gnu"* ]]; then
  # Linux
  export SSH_ASKPASS="/usr/bin/ssh-askpass"
  eval `keychain --agents gpg,ssh --eval id_ed25519 66832BC1`
else
  # OS X
    if ! ssh-add -l | grep '\.ssh/kevel\.pem' > /dev/null
    then
        ssh-add ~/.ssh/kevel.pem
    fi
    if ! ssh-add -l | grep 'ED25519' > /dev/null
    then
        ssh-add ~/.ssh/id_ed25519
    fi
fi

# https://metaredux.com/posts/2020/07/07/supercharge-your-bash-history.html
shopt -s checkwinsize histappend
export HISTCONTROL=ignoreboth
export HISTFILESIZE=10000000
export HISTSIZE=100000
export HISTIGNORE='ls:ll:cd:pwd:bg:fg:history'
# Not enabling this since i think direnv uses PROMPT_COMMAND
# PROMPT_COMMAND="history -a; history -n"

# set a fancy prompt (non-color, unless we know we "want" color)
case "$TERM" in
    xterm-color|*-256color) color_prompt=yes;;
esac

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
  elif [ -f /usr/local/etc/profile.d/bash_completion.sh ]; then
    . /usr/local/etc/profile.d/bash_completion.sh
  fi
fi

parse_git_branch() {
  git branch 2> /dev/null | sed -e '/^[^*]/d' -e 's/* \(.*\)/(\1)/'
}
PS1="${yellow}\$AWS_PROFILE${white}:\w\$(parse_git_branch)[L\$SHLVL] $ "
show_virtual_env() {
  if [[ -n "$VIRTUAL_ENV" ]]; then
    echo "($(basename $VIRTUAL_ENV))"
  fi
}
export -f show_virtual_env
PS1='$(show_virtual_env)'$PS1

export EDITOR=emacsclient
export VISUAL=$EDITOR
export ALTERNATE_EDITOR=emacs
export CVS_RSH=ssh
export PAGER=most

# asdf
if [ -f "$HOME/.asdf/asdf.sh" ]; then
  source "$HOME/.asdf/asdf.sh"
  source "$HOME/.asdf/completions/asdf.bash"
  source "$HOME/.asdf/plugins/java/set-java-home.bash"
  # clojure installer requires this: https://github.com/clojure/brew-install/commit/f238dcebaa0b7bce51305c322dfb89fd3e152304
  export HOMEBREW_RUBY_PATH=ruby
fi

# direnv
if [ -n "$(which direnv)" ]; then
    eval "$(direnv hook bash)"
fi

if [ -f "$HOME/.adzerk/env.gpg" ]; then
  if [[ -z "$AWS_PROFILE" ]]; then
    export AWS_PROFILE=default
  fi
  export KEVEL_JHA_READONLY_PROFILE=jha
  eval "$(gpg -d ~/.adzerk/env.gpg)"
fi

### START PACS STUFF ####
### https://github.com/adzerk/pacs/blob/main/scripts/pacs.md

# inspect the aws sso cache to see if our token is expired. macos users will
# probably need to use `gdate` instead of `date`
am-i-logged-in-to-aws () {
  local sso_home=~/.aws/sso/cache
  local logged_in=false
  if [[ ! -d $sso_home ]]; then
    echo $logged_in
    return
  fi
  for f in $(ls $sso_home | grep -v boto); do
    expires="$(gdate -d "$(cat $sso_home/$f | jt expiresAt %)" +%s)"
    now="$(gdate +%s)"
    # if the expiration of our sso cache has passed, delete it
    if [[ $now -ge $expires ]]; then
      rm $sso_home/$f
    else
      logged_in=true
    fi
  done
  echo $logged_in
}

# aws profile switcher
#
# sets AWS_PROFILE, and optionally TICKET
# if escalated privs are being used, then requires TICKET
#
# Finally, calls `aws sso login` if needed
awp () {
  local profile=$1
  local ticket=$2
  export AWS_PROFILE=$profile
  export TICKET=$ticket
  if [[ ! $profile ]]; then
    echo "provide a profile 'awp default'"
    return
  fi
  if [[ $profile == ESCALATED-* ]]; then
    if [[ -z "$TICKET" ]]; then
      echo "set TICKET env or provide it for escalated privs: 'awp ESCALATED-jha 12345'"
      return
    fi
  fi
  if [[ $(am-i-logged-in-to-aws) == "false" ]]; then
    aws sso login --profile $profile
  fi
}

# autocomplete for awp, inspects your .aws/config file for suggestions, macos
# users might need `gsed` here instead of `sed`
_awp() {
  local cur=${COMP_WORDS[COMP_CWORD]}
  local ses=$(cat ~/.aws/config | grep '^\[' | cut -f2 -d' ' | gsed 's/\]//g')
  COMPREPLY=( $(compgen -W "$ses" -- $cur) )
}
complete -F _awp awp

### END PACS STUFF ####

# https://github.com/akermu/emacs-libvterm#shell-side-configuration
vterm_printf(){
    if [ -n "$TMUX" ] && ([ "${TERM%%-*}" = "tmux" ] || [ "${TERM%%-*}" = "screen" ] ); then
        # Tell tmux to pass the escape sequences through
        printf "\ePtmux;\e\e]%s\007\e\\" "$1"
    elif [ "${TERM%%-*}" = "screen" ]; then
        # GNU screen (screen, screen-256color, screen-256color-bce)
        printf "\eP\e]%s\007\e\\" "$1"
    else
        printf "\e]%s\e\\" "$1"
    fi
}
if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
    function clear(){
        vterm_printf "51;Evterm-clear-scrollback";
        tput clear;
    }
fi
vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
}
PS1=$PS1'\[$(vterm_prompt_end)\]'
