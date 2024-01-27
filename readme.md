# Dotfiles
Configuration files for a macOS system with [nix darwin](https://github.com/LnL7/nix-darwin) and emacs.

Use the determinate systems nix installer: `curl --proto '=https' --tlsv1.2 -sSf -L https://install.determinate.systems/nix | sh -s -- install`

## Nix
When the configuration is changed, run:
`nix run nix-darwin -- switch --flake ~/.config/nix-darwin `
