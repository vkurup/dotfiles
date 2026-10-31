# -*- mode: sh -*-

alias ll="ls -al"
alias la="ls -A"
alias l="ls -CF"
alias lh="ls -l -h"

alias mv='mv -i'

alias grep="rg"

alias f="find"

# git
alias gst="git status"
alias gbv="git branch -v"
alias gb="git branch"
alias gco="git checkout"
alias glt="git log -n 10"
alias gsu="git submodule update --init --recursive"

alias top="htop"
alias github='br=$(git branch --contains HEAD | sed -rn "s/^\* //p"); if ! git ls-remote . | grep -q -e "refs/remotes/.*/${br}"; then br="master"; fi; xdg-open $(git config -l | sed -rn "s%remote.origin.url=git(@|://)(github.com)(:|/)(.+/.+).git%https://\2/\4/tree/${br}%p")'
alias gitrd='git rm $(git ls-files -d)'
alias pm='python manage.py'
alias pmt='REUSE_DB=1 python manage.py test'
alias hpr='hub pull-request -b develop -i '

alias less='most'

# emacs
alias e="emacsclient -t"
alias E="SUDO_EDITOR=\"emacsclient -t -a emacs\" sudoedit"

# k8s
alias k="kubectl"

# Add an "alert" alias for long running commands.  Use like so:
#   sleep 10; alert
alias alert='notify-send --urgency=low -i "$([ $? = 0 ] && echo terminal || echo error)" "$(history|tail -n1|sed -e '\''s/^\s*[0-9]\+\s*//;s/[;&|]\s*alert$//'\'')"'

# aws
alias awsvk="aws --cli-auto-prompt"
alias sso="aws sso login"

# clojure
alias clj="clojure -M:rebel"
