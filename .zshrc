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

export EDITOR='emacs'

# Modules
# -------
# Different modules exist to handle orthogonal functionalities.
# This allows for easy on/off using comments.
#
# Each module essentially does not depend on others.
configs=(
  conda.sh
  emacs.sh
  # gcp.sh
  git.sh
  go.sh
  jenv.sh
  mysql.sh
  nvm.sh
  pnpm.sh
  pyenv.sh
  # rbenv.sh
  spaceship.sh
  volta.sh
  bb.sh         # Has to be after one of the above. Unclear which.
)
for module in "${configs[@]}"
do
  source "$ZSH_FILES_PATH/$module"
done

alias ll="exa -l"
alias ls=exa
alias cat=bat
alias grep=rg
