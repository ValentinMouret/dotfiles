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

function fonts::download() {
  readonly font_archive="$1"
  readonly font_url="$https://download.jetbrains.com/fonts/$font_archive"
  curl -fosSL "$font_url"
}

function fonts::install() {
  readonly font_archive="$1"
  unzip font_archive
  readonly folder_name=$(echo "$font_archive" | sed 's/\.zip//g')
  for f in $folder_name/*.tff;do
    cp -vf "$f" ~/Library/Fonts
  done
}

function fonts::setup() {
  fonts::download && fonts::install
}

brew::install_if_absent &&
  brew::install_packages

echo "Copying the fish configuration."
cp -r fish_config ~/.config/fish

fonts::setup
