#
# ~/.bash_profile
#

[[ -f ~/.bashrc ]] && . ~/.bashrc

eval $(keychain --eval -Q --quiet id_rsa)
