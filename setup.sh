#!/bin/bash

set -euo pipefail


function brew::install() {
  readonly brew_url="https://raw.githubusercontent.com/Homebrew/install/master/install"
  /usr/bin/ruby -e "$(curl -fsSL $brew_url)"
}

function brew::install_if_absent() {
  if [ -z $(command -v brew) ];then
    brew::install
  fi
}

function brew::install_packages() {
	brew bundle
}

brew::install_if_absent &&
  brew::install_packages

echo "Copying the fish configuration."
cp -r fish_config ~/.config/fish
