#!/bin/zsh

alias ga="git add"
alias gs="git status"
alias gc="git commit"
alias gca="git commit --amend"
alias gcp="git cherry-pick"
alias gl="git prettylog"
alias gd="git diff"
alias gp="git push"

git config --global alias.prettylog "log --graph --pretty=format:'%Cred%h%Creset -%C(yellow)%d%Creset %s %Cgreen(r) %C(bold blue)<%an>%Creset' --abbrev-commit --date=relative"
git config --global alias.root "rev-parse --show-toplevel"
