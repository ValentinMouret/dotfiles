{ config, pkgs, ... }:

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

  # Let Home Manager install and manage itself.
  programs = {
    home-manager.enable = true;

    direnv = {
      enable = true;
      nix-direnv.enable = true;
      enableZshIntegration = true;
    };

    zsh.enable = true;


    git = {
      enable = true;
      userName = "Valentin Mouret";
      userEmail = "valentin.mouret@hey.com";
      extraConfig = {
        core = {
          pager = "delta";
        };
        diff = {
          tool = "delta";
          navigate = true;
          light = false;
          colorMoved = "default";
        };
        delta = {
          side-by-side = true;
          line-numbers = true;
        };
        interactive = {
          diffFilter = "delta --color-only";
        };
        merge = {
          conflictstyle = "diff3";
        };
        pull = {
          rebase = true;
        };
        rerere = {
          enabled = true;
        };
      };
    };

    starship = {
        enable = true;
      };
  };
}
