#!/bin/bash

# Runs the different setups.

set -euo pipefail


function brew::install() {
  readonly brew_url="https://raw.githubusercontent.com/Homebrew/install/master/install"
  /usr/bin/ruby -e "$(curl -fsSL $brew_url)"
}

function brew::install_if_absent() {
  if [ -z "$(command -v brew)" ]; then
    brew::install
  fi
}

function brew::install_packages() {
  brew bundle
}

function brew::setup() {
    brew::install_if_absent &&
        brew:install_packages
}

function emacs::setup() {
    emacs_dir_path="$HOME/.emacs.d"
    if [ -d emacs_dir_path ]; then
        rm -rf "$emacs_dir_path"
    fi
    mkdir -p "$emacs_dir_path"
    ln -sf "./setup" "$emacs_dir_path"
    ln -sf "./config" "$emacs_dir_path"
    ln -sf "./init.el" "$emacs_dir_path/init.el"
}

echo "Setting up Homebrew"
brew::setup

echo "Setting up Emacs"
emacs::setup

echo "Copying the fish configuration."
cp -r fish_config ~/.config/fish
