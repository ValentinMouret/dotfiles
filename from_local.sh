#!/bin/zsh

# Copies the local emacs and zsh setup to the local dotfiles repository.

DEV_PATH=${DEV_PATH:-$HOME/Developer/valentinmouret}
DOTFILES_PATH=$DEV_PATH/dotfiles

if ! [[ -d $DOTFILES_PATH ]]; then
  echo "$DOTFILES_PATH does not exist"
  exit 1
fi

EMACS_PATH=$HOME/.emacs.d

cp $EMACS_PATH/init.el $DOTFILES_PATH/.emacs.d/init.el
cp $EMACS_PATH/early-init.el $DOTFILES_PATH/.emacs.d/early-init.el

rm $DOTFILES_PATH/.emacs.d/elisp/*
cp $EMACS_PATH/elisp/*.el $DOTFILES_PATH/.emacs.d/elisp/
