# Set the PATH
export PATH=/opt/homebrew/bin:$PATH
export PATH=$PATH:$HOME/bin                # User scripts
export PATH=$PATH:/opt/local/bin           # Homebrew Apple Silicon
export PATH=$PATH:$HOME/bin:/usr/local/bin # Classic Homebrew

# Variables
# ---------
# CODE_PATH: root location of repositories
#            they reprocude a simple git like owner/repo
CODE_PATH=$HOME/Developer
# DEV_HOME: Path with all my source code
export DEV_HOME=~/Developer/valentinmouret

# ZSH_FILES: folder with additional zsh configuration.
ZSH_FILES_PATH=$HOME/.zsh

# Enable autocomplete
# source $CODE_PATH/marlonrichert/zsh-autocomplete/zsh-autocomplete.plugin.zsh

# Modules
# -------
# Different modules exist to handle orthogonal functionalities.
# This allows for easy on/off using comments.
#
# Each module essentially does not depend on others.
configs=(
  conda.sh
  git.sh
  go.sh
  jenv.sh
  mysql.sh
  nvm.sh
  pnpm.sh
  pyenv.sh
  rust.sh
  spaceship.sh
  volta.sh
  bb.sh         # Has to be after one of the above. Unclear which.
  nvim.sh
)
for module in "${configs[@]}"
do
  source "$ZSH_FILES_PATH/$module"
done

# General aliases
alias ll="exa -l"
alias ls=exa
alias cat=bat
alias grep=rg
eval "$(atuin init zsh)"

export EDITOR="emacs"

export PUPPETEER_SKIP_CHROMIUM_DOWNLOAD=true
export PUPPETEER_EXECUTABLE_PATH=`which chromium`
export PATH="/opt/homebrew/opt/postgresql@15/bin:$PATH"

export LANG=en_US.UTF-8
export LC_ALL=en_US.UTF-8


[ -f "/Users/valentin/.ghcup/env" ] && source "/Users/valentin/.ghcup/env" # ghcup-env