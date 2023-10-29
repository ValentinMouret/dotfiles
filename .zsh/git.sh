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

function parse_dev_path {
  # Parses a string passed as an argument that is either "owner/repo" or just "repo" into two variables.
  # If the owner is not present, it should default to "valentinmouret"
  input_string=$1

  # test if the string contains a "/"
  if [[ $input_string == *"/"* ]]; then
    readonly owner=$(echo $1 | sed -E "s/(.*)\/(.*)/\1/" || echo "valentinmouret")
    readonly repo_name=$(echo $1 | sed -E "s/(.*)\/(.*)/\2/" || echo $1)
  else
    readonly owner="valentinmouret"
    readonly repo_name=$1
  fi

  # returns the two strings joined with /
  echo "$owner/$repo_name"
}

function github::clone_repo {
  readonly dev_path=$(parse_dev_path $1)
  readonly git_path="git@github.com:$dev_path"
  git clone $git_path "/Users/valentin/Developer/$dev_path"
}
