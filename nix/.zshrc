export PYENV_ROOT="$HOME/.pyenv"
[[ -d $PYENV_ROOT/bin ]] && export PATH="$PYENV_ROOT/bin:$PATH"
eval "$(pyenv init -)"
eval $(starship init zsh)

plugins=(git fzf)

export FZF_DEFAULT_COMMAND='ag --hidden -g ""'

function github::clone_repo {
  readonly dev_path=$(python3 $HOME/.config/nix-darwin/utils.py $1)
  readonly git_path="git@github.com:$dev_path"
  git clone $git_path "/Users/valentinmouret/Developer/$dev_path"
}

alias psql_production="psql -h localhost -p 10000 -U kiosk_back__9899"
alias psql_staging="psql -h localhost -p 10000 -U kiosk_back__9899"
alias psql_greenster="psql -h localhost -p 10000 -U kiosk_back__1699"
