# OPENSPEC:START
# OpenSpec shell completions configuration
if [ -d "$HOME/.local/share/bash-completion/completions" ]; then
  for f in "$HOME/.local/share/bash-completion/completions"/*; do
    [ -f "$f" ] && . "$f"
  done
fi
# OPENSPEC:END

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
if [[ "$OSTYPE" == "darwin"* ]]; then
  PS1="${yellow}\$AWS_PROFILE\${TICKET:+(\$TICKET)}${white}:\w\$(parse_git_branch)[L\$SHLVL] $ "
else
  PS1="${yellow}\${AWS_PROFILE:+\$AWS_PROFILE }${white}\u@\h:\w\$(parse_git_branch)[L\$SHLVL] $ "
fi
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

# Homebrew
[ -x /opt/homebrew/bin/brew ] && eval "$(/opt/homebrew/bin/brew shellenv)"
# export PATH="/opt/homebrew/opt/gnupg@2.2/bin:$PATH"

# version manager: mise if installed, otherwise asdf
if command -v mise > /dev/null; then
  eval "$(mise activate bash)"
elif [ -f "$HOME/.asdf/asdf.sh" ]; then
  # asdf < 0.16 (shell-script version)
  source "$HOME/.asdf/asdf.sh"
  source "$HOME/.asdf/completions/asdf.bash"
  [ -f "$HOME/.asdf/plugins/java/set-java-home.bash" ] && source "$HOME/.asdf/plugins/java/set-java-home.bash"
elif [ -d "$HOME/.asdf" ] && command -v asdf > /dev/null; then
  # asdf >= 0.16 (Go binary)
  export ASDF_DATA_DIR="$HOME/.asdf"
  export PATH="$ASDF_DATA_DIR/shims:$PATH"
  source <(asdf completion bash)
fi
# clojure installer requires this: https://github.com/clojure/brew-install/commit/f238dcebaa0b7bce51305c322dfb89fd3e152304
if [[ "$OSTYPE" == "darwin"* ]]; then
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

# Select a Jira ticket from the current branch or from assigned Crushing/Waiting
# work, then enable PACS access in this shell.
_escalate_usage() {
  cat <<'EOF'
Usage: ee [options] [TICKET]

By default, ee extracts a Jira ticket from the current Git branch and
runs the equivalent of:

  awp ESCALATED-jha TICKET

If the branch has no Jira ticket, it prompts you to select one of your assigned
Crushing or Waiting tickets.

Options:
      --aws                 Use AWS (default)
      --gcp                 Use GCP
  -t, --ticket TICKET       Use TICKET instead of detecting or selecting one
  -p, --profile PROFILE     AWS profile (default: ESCALATED-jha)
  -P, --project PROJECT     GCP project (default: current gcloud project)
  -a, --account ACCOUNT     GCP account (default: pacs-gcp auto-detection)
  -r, --role ROLE           GCP role (default: engineer)
  -g, --organization        Request GCP organization-wide access
      --list-profiles       List configured ESCALATED-* AWS profiles
  -h, --help                Show this help

Defaults can be configured with ESCALATE_CLOUD, ESCALATE_AWS_PROFILE,
ESCALATE_GCP_PROJECT, ESCALATE_GCP_ACCOUNT, and ESCALATE_GCP_ROLE.

Examples:
  ee
  ee MGMT-1045
  ee --profile ESCALATED-tooling
  ee --gcp --project management-prod-473909
  ee --gcp --organization --role admin
EOF
}

_escalate_ticket_from_branch() {
  local branch ticket

  branch=$(git branch --show-current 2>/dev/null) || return 1
  [[ -n "$branch" ]] || return 1

  if [[ "$branch" =~ ([[:alpha:]][[:alnum:]]*-[[:digit:]]+) ]]; then
    ticket=${BASH_REMATCH[1]}
  else
    return 1
  fi

  printf '%s\n' "${ticket^^}"
}

_escalate_aws_profile_exists() {
  local wanted=$1 configured

  while IFS= read -r configured; do
    [[ "$configured" == "$wanted" ]] && return 0
  done < <(aws configure list-profiles)
  return 1
}

_escalate_select_ticket() {
  local output choice index row key status summary
  local -a rows

  if [[ ! -t 0 ]]; then
    echo "ee: no ticket in the current branch and no terminal available for selection" >&2
    return 1
  fi
  if ! command -v twg >/dev/null 2>&1; then
    echo "ee: twg is not in PATH" >&2
    return 127
  fi
  if ! command -v jq >/dev/null 2>&1; then
    echo "ee: jq is not in PATH" >&2
    return 127
  fi

  output=$(mktemp "${TMPDIR:-/tmp}/escalate.XXXXXX") || return 1
  if ! twg jira workitem query \
      --jql 'assignee = currentUser() AND status in ("Crushing", "Waiting") ORDER BY status ASC, key ASC' \
      --fields key,summary,status \
      --limit 100 \
      --output json \
      --output-summary none \
      --output-file "$output" >/dev/null; then
    rm -f "$output"
    echo "ee: could not retrieve Jira tickets" >&2
    return 1
  fi

  mapfile -t rows < <(
    jq -r '.data.issues[] | [.key, .status.name, ((.summary // "") | gsub("[\\r\\n\\t]+"; " "))] | @tsv' "$output"
  )
  rm -f "$output"

  if (( ${#rows[@]} == 0 )); then
    echo "ee: no Jira tickets assigned to you are Crushing or Waiting" >&2
    return 1
  fi

  echo "No Jira ticket found in the current branch. Select a ticket:" >&2
  {
    printf '#\tTicket\tStatus\tTitle\n'
    for index in "${!rows[@]}"; do
      IFS=$'\t' read -r key status summary <<< "${rows[$index]}"
      printf '%d\t%s\t%s\t%s\n' "$((index + 1))" "$key" "$status" "$summary"
    done
  } | column -t -s $'\t' >&2

  while true; do
    read -r -p "Ticket number (q to cancel): " choice
    case "$choice" in
      q|Q) return 1 ;;
      ''|*[!0-9]*) echo "Enter a number from 1 to ${#rows[@]}, or q." >&2 ;;
      *)
        if (( choice >= 1 && choice <= ${#rows[@]} )); then
          IFS=$'\t' read -r key status summary <<< "${rows[$((choice - 1))]}"
          printf '%s\n' "$key"
          return 0
        fi
        echo "Enter a number from 1 to ${#rows[@]}, or q." >&2
        ;;
    esac
  done
}

ee() {
  local cloud=${ESCALATE_CLOUD:-aws}
  local aws_profile=${ESCALATE_AWS_PROFILE:-ESCALATED-jha}
  local gcp_project=${ESCALATE_GCP_PROJECT:-}
  local gcp_account=${ESCALATE_GCP_ACCOUNT:-}
  local gcp_role=${ESCALATE_GCP_ROLE:-}
  local gcp_organization=false
  local ticket='' branch='' gcp_configuration status
  local -a pacs_args

  while (( $# > 0 )); do
    case "$1" in
      --aws) cloud=aws ;;
      --gcp) cloud=gcp ;;
      -t|--ticket)
        [[ $# -ge 2 ]] || { echo "ee: $1 requires a ticket" >&2; return 2; }
        ticket=$2
        shift
        ;;
      -p|--profile)
        [[ $# -ge 2 ]] || { echo "ee: $1 requires a profile" >&2; return 2; }
        aws_profile=$2
        shift
        ;;
      -P|--project)
        [[ $# -ge 2 ]] || { echo "ee: $1 requires a project" >&2; return 2; }
        gcp_project=$2
        shift
        ;;
      -a|--account)
        [[ $# -ge 2 ]] || { echo "ee: $1 requires an account" >&2; return 2; }
        gcp_account=$2
        shift
        ;;
      -r|--role)
        [[ $# -ge 2 ]] || { echo "ee: $1 requires a role" >&2; return 2; }
        gcp_role=$2
        shift
        ;;
      -g|--organization)
        cloud=gcp
        gcp_organization=true
        ;;
      --list-profiles)
        while IFS= read -r aws_profile; do
          [[ "$aws_profile" == ESCALATED-* ]] && printf '%s\n' "$aws_profile"
        done < <(aws configure list-profiles)
        return 0
        ;;
      -h|--help)
        _escalate_usage
        return 0
        ;;
      -*)
        echo "ee: unknown option: $1" >&2
        _escalate_usage >&2
        return 2
        ;;
      *)
        if [[ -n "$ticket" ]]; then
          echo "ee: unexpected argument: $1" >&2
          return 2
        fi
        ticket=$1
        ;;
    esac
    shift
  done

  if [[ -z "$ticket" ]]; then
    branch=$(git branch --show-current 2>/dev/null || true)
    if ticket=$(_escalate_ticket_from_branch); then
      echo "Using $ticket from branch '$branch'."
    else
      ticket=$(_escalate_select_ticket) || return
    fi
  fi

  ticket=${ticket#jira-}
  ticket=${ticket#JIRA-}
  ticket=${ticket^^}
  if [[ ! "$ticket" =~ ^[[:alpha:]][[:alnum:]]*-[[:digit:]]+$ ]]; then
    echo "ee: invalid Jira ticket: $ticket" >&2
    return 2
  fi

  case "$cloud" in
    aws)
      if ! command -v awp >/dev/null 2>&1; then
        echo "ee: awp is not available in this shell" >&2
        return 127
      fi
      if ! _escalate_aws_profile_exists "$aws_profile"; then
        echo "ee: AWS profile '$aws_profile' is not configured" >&2
        echo "Add or uncomment [profile $aws_profile] in ~/.aws/config." >&2
        return 2
      fi
      awp "$aws_profile" "$ticket"
      status=$?
      if (( status == 0 )); then
        echo "AWS escalation configured with $aws_profile for $ticket."
      fi
      return "$status"
      ;;
    gcp)
      if ! command -v pacs-gcp >/dev/null 2>&1; then
        echo "ee: pacs-gcp is not in PATH" >&2
        return 127
      fi

      pacs_args=(-t "$ticket")
      if [[ "$gcp_organization" == true ]]; then
        pacs_args+=(-g)
        gcp_configuration=pacs-org
      else
        if [[ -z "$gcp_project" ]]; then
          gcp_project=$(gcloud config get-value project 2>/dev/null)
        fi
        if [[ -z "$gcp_project" || "$gcp_project" == '(unset)' ]]; then
          echo "ee: no GCP project selected; use --project" >&2
          return 2
        fi
        pacs_args+=(-p "$gcp_project")
        gcp_configuration="pacs-$gcp_project"
      fi
      [[ -n "$gcp_account" ]] && pacs_args+=(-a "$gcp_account")
      [[ -n "$gcp_role" ]] && pacs_args+=(-r "$gcp_role")

      (
        unset CLOUDSDK_ACTIVE_CONFIG_NAME
        pacs-gcp "${pacs_args[@]}"
      )
      status=$?
      if (( status == 0 )); then
        export CLOUDSDK_ACTIVE_CONFIG_NAME=$gcp_configuration
        export TICKET=$ticket
        echo "GCP escalation configured with $gcp_configuration for $ticket."
      fi
      return "$status"
      ;;
    *)
      echo "ee: unsupported cloud '$cloud' (expected aws or gcp)" >&2
      return 2
      ;;
  esac
}

# Keep the longer name working for existing shells and muscle memory.
escalate() {
  ee "$@"
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

export PATH="$HOME/.local/bin:$PATH"

export NO_SPEAK=1
# opencode
[ -d "$HOME/.opencode/bin" ] && export PATH="$HOME/.opencode/bin:$PATH"

if [[ "$OSTYPE" == "darwin"* ]]; then
  # openjml
  export PATH=$HOME/src/openjml-macos-14-21-0.23:$PATH
  # docker
  export PATH="$PATH:/Applications/Docker.app/Contents/Resources/bin/"
fi
