#!/bin/zsh

# Copies the local emacs and zsh setup to the local dotfiles repository.

set -euo pipefail

DEV_PATH=${DEV_PATH:-$HOME/Developer/valentinmouret}
DOTFILES_PATH=$DEV_PATH/dotfiles

function copy::emacs () {
  if ! [[ -d $DOTFILES_PATH ]]; then
    echo "$DOTFILES_PATH does not exist"
    exit 1
  fi

  EMACS_PATH=$HOME/.emacs.d

  cp $EMACS_PATH/init.el $DOTFILES_PATH/.emacs.d/init.el
  cp $EMACS_PATH/early-init.el $DOTFILES_PATH/.emacs.d/early-init.el

  rm $DOTFILES_PATH/.emacs.d/elisp/*
  cp $EMACS_PATH/elisp/*.el $DOTFILES_PATH/.emacs.d/elisp/
}

function copy::zsh () {
  cp $HOME/.zshrc $DOTFILES_PATH/.zshrc
  zsh_modules_path=$HOME/.zsh
  
  cp $HOME/.zsh/* $DOTFILES_PATH/.zsh
}

function copy::helix () {
  cp $HOME/.config/helix/config.toml $DOTFILES_PATH/.config/helix/config.toml
  cp $HOME/.config/helix/languages.toml $DOTFILES_PATH/.config/helix/languages.toml
}

# copy::emacs && copy::zsh && copy::helix
copy::helix
