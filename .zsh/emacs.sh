#!/bin/zsh

emacs_path=/Applications/Emacs.app/Contents/MacOS/Emacs

function emacs () {
  $emacs_path $@ &
}

alias emacsnw="$emacs_path -nw"
