# -*- mode: sh -*-
[ "$EMACS" == "t" ] || alias ls="ls --color"

alias ll="ls -alF"
alias la="ls -A"
alias l="ls -CF"
alias lh="ls -l -h"

alias mv='mv -i'

alias grep="grep --color=auto"

alias f="find"

# package management
alias pmi="sudo pacmatic -S"
alias pmr="sudo pacmatic -Rs"
alias pmu="sudo pacman -Sc && sudo pacmatic -Syu"
alias pms="pacmatic -Ss"
alias pmsh="pacmatic -Qi"
alias pmclean='sudo pacmatic -Rsn $(pacman -Qdtq)'

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
