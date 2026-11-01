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

if [[ "$OSTYPE" == "darwin"* ]]; then
    # brew psql 15
    export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"

    # brew mssql-tools
    export PATH="/opt/homebrew/opt/mssql-tools18/bin:$PATH"
    #export PATH="/usr/local/opt/mssql-tools18/bin:$PATH"
fi

# Can't do this because our scripts expect a Mac sed if OS is Darwin
# gnu-sed instead of sed (from `brew info gnu-sed`)
#export PATH="/usr/local/opt/gnu-sed/libexec/gnubin:$PATH"

# Kevel
export PATH=$PATH:~/dev/teammgmt/bin:~/dev/teammgmt/infrastructure/bin:~/dev/cli-tools/micha:~/dev/cli-tools/scripts

source "$HOME/.bashrc"

test -e "${HOME}/.iterm2_shell_integration.bash" && source "${HOME}/.iterm2_shell_integration.bash"

# uv
export PATH="$HOME/.local/bin:$PATH"

# node
export PATH="$HOME/node_modules/.bin:$PATH"
export PATH="$PWD/node_modules/.bin:$PATH"

# The next line updates PATH for the Google Cloud SDK.
if [ -f '/opt/homebrew/share/google-cloud-sdk/path.bash.inc' ]; then . '/opt/homebrew/share/google-cloud-sdk/path.bash.inc'; fi

# The next line enables shell command completion for gcloud.
if [ -f '/opt/homebrew/share/google-cloud-sdk/completion.bash.inc' ]; then . '/opt/homebrew/share/google-cloud-sdk/completion.bash.inc'; fi

# bun
export BUN_INSTALL="$HOME/.bun"
export PATH="$BUN_INSTALL/bin:$PATH"
# mise is activated in .bashrc
