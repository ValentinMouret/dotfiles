{ config, pkgs, ... }:

let
  emacs-overlay = import (fetchTarball {
    url = "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
    sha256 = "5b263d5788c503a83399c2b54ba87e46fb96b418";
  });
  my-emacs = pkgs.emacs29.override {
    withNativeCompilation = true;
    withSQLite3 = true;
    withTreeSitter = true;
    withWebP = true;
  };
  my-emacs-with-packages = (pkgs.emacsPackagesFor my-emacs).emacsWithPackages (epkgs: with epkgs; [
    pkgs.mu
    vterm
    multi-vterm
    pdf-tools
    treesit-grammars.with-all-grammars
  ]);
in
{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "valentinmouret";
  # home.homeDirectory = "/Users/valentinmouret";

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "23.11";

  home.packages = with pkgs; [
    emacs-all-the-icons-fonts
    (aspellWithDicts (d: [d.en d.sv]))
    ghostscript
    tetex
    poppler
    mu
    wordnet
  ];

  # Let Home Manager install and manage itself.
  programs = {
    emacs = {
      enable = true;
      package = my-emacs-with-packages;
    };
    home-manager.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
      enableZshIntegration = true;
    };

    zsh = {
      enable = true;
      enableCompletion = true;
      history = {
        ignoreAllDups = true;
      };
      initExtra = builtins.readFile (./.zshrc);
    };

    nushell = {
      enable = true;
    };

    java = {
      enable = true;
    };

    git = {
      enable = true;
      userName = "Valentin Mouret";
      userEmail = "valentin.mouret@hey.com";
      extraConfig = {
        github = {
          user = "ValentinMouret";
        };
        diff = {
          navigate = true;
          light = false;
          colorMoved = "default";
        };
        delta = {
          side-by-side = true;
          line-numbers = true;
        };
        merge = {
          conflictstyle = "diff3";
        };
        pull = {
          rebase = true;
        };
        push = {
          autoSetupRemote = true;
        };
        rerere = {
          enabled = true;
        };
      };
    };

    starship = {
      enable = true;
      settings = {
        add_newline = false;
        character = {
          success_symbol = "[➜](bold green)";
          error_symbol = "[➜](bold red)";
        };
      };
    };
  };
}
