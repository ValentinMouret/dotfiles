{
  description = "Valentin’s nix darwin flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

    # Check https://status.nixos.org
    nix-darwin.url = "github:LnL7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # Home-manager stuff
    home-manager.url = "github:nix-community/home-manager/master";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nix-darwin, home-manager, ... }:
    let
      configuration = { pkgs, ... }: {
        # List packages installed in system profile. To search by name, run:
        # $ nix-env -qaP | grep wget
        nixpkgs.config.allowUnfree = true;

        environment.systemPackages = with pkgs;
          [
            aider-chat
            bat
            cargo
            claude-code
            clojure
            clojure-lsp
            cmake
            devenv
            difftastic
            direnv
            # CLI for building, runing, testing, and managing your Emacs Lisp dependencies
            # https://emacs-eask.github.io
            eza
            fd
            fzf
            git
            gitAndTools.gh
            helix
            ispell
            iosevka
            glibtool
            libtool
            jdt-language-server
            nerd-fonts.jetbrains-mono
            jujutsu
            multimarkdown
            nil # Nix lsp
            nixpkgs-fmt
            # nodePackages."bash-language-server"
            nodePackages."dockerfile-language-server-nodejs"
            nodePackages."graphql-language-service-cli"
            nodePackages."prettier"
            nodePackages."typescript"
            nodePackages."typescript-language-server"
            nodePackages."vscode-langservers-extracted"
            nodePackages."localtunnel"
            nodePackages.pnpm
            nodejs
            jdk23
            pyenv
            rage
            rust-analyzer
            rustc
            rustfmt
            ripgrep
            silver-searcher
            terraform-ls
            tree-sitter
            tree-sitter-grammars.tree-sitter-clojure
            tree-sitter-grammars.tree-sitter-go
            tree-sitter-grammars.tree-sitter-python
            tree-sitter-grammars.tree-sitter-tsx
            tree-sitter-grammars.tree-sitter-typescript
          ];
        environment.shellAliases = {
          emacs = "open -a Emacs";

          ls = "eza";
          ll = "ls -l";
          grep = "rg";
          cat = "bat";

          gs = "git status";
          ga = "git add";
          gc = "git commit";
          gd = "git diff";
          gl = "git log";
          gp = "git push";
        };
        environment.variables = {
          LANG = "en_GB.UTF-8";
          LC_ALL = "en_GB.UTF-8";
          EDITOR = "hx";
          BAT_THEME = "Nord";
          # RUST_SRC_PATH = "${pkgs.latest.rustChannels.stable.rust-src}/lib/rustlib/src/rust/library/";
        };
        environment.pathsToLink = [ "/share/zsh" ];

        fonts.packages = with pkgs; [
          iosevka
          jetbrains-mono
        ];

        # Auto upgrade nix package and the daemon service.
        # nix.package = pkgs.nix;

        # Necessary for using flakes on this system.
        nix.settings.experimental-features = "nix-command flakes";
        # nix.optimise.automatic = true;

        # Create /etc/zshrc that loads the nix-darwin environment.
        programs.zsh = {
          enable = true; # default shell on catalina
          enableFzfCompletion = true;
          enableFzfGit = true;
          enableFzfHistory = true;
          enableSyntaxHighlighting = true;
        };

        services.tailscale = {
          enable = true;
        };

        services.postgresql = {
          enable = true;
        };

        # Set Git commit hash for darwin-version.
        system.configurationRevision = self.rev or self.dirtyRev or null;

        # Used for backwards compatibility, please read the changelog before changing.
        # $ darwin-rebuild changelog
        system.stateVersion = 4;

        system.primaryUser = "valentinmouret";

        system.defaults.".GlobalPreferences"."com.apple.mouse.scaling" = 10.0;
        system.defaults.NSGlobalDomain.AppleInterfaceStyleSwitchesAutomatically = true;
        system.defaults.NSGlobalDomain.InitialKeyRepeat = null;
        system.defaults.NSGlobalDomain.KeyRepeat = null;

        system.defaults.NSGlobalDomain.NSAutomaticCapitalizationEnabled = false;
        system.defaults.NSGlobalDomain.NSAutomaticSpellingCorrectionEnabled = false;
        system.defaults.NSGlobalDomain."com.apple.mouse.tapBehavior" = 1;
        system.defaults.NSGlobalDomain."com.apple.trackpad.scaling" = 100.0;

        system.defaults.dock.autohide = true;
        system.defaults.trackpad.Clicking = true;
        system.keyboard.enableKeyMapping = true;
        system.keyboard.remapCapsLockToControl = true;

        # The platform the configuration will be used on.
        nixpkgs.hostPlatform = "aarch64-darwin";

        # Necessary for home-manager to work
        users.users.valentinmouret.home = "/Users/valentinmouret";

        homebrew = {
          onActivation = {
            autoUpdate = true;
            upgrade = true;
            cleanup = "uninstall";
          };
          enable = true;
          brews = [
            {
              name = "postgresql@14";
              restart_service = true;
              link = true;
            }
            # "emacs-plus"
          ];
          taps = [
            # "d12frosted/emacs-plus"
          ];
          casks = [
            "brave-browser"
            "chatgpt"
            "claude"
            "cursor"
            "cyberduck"
            "discord"
            "emacs"
            "figma"
            "ghostty"
            "google-chrome"
            "hey"
            "linear-linear"
            "lm-studio"
            "notion"
            "ollama"
            "orbstack"
            "postico"
            "rectangle"
            "roam-research"
            "signal"
            "slack"
            "visual-studio-code"
            "whatsapp"
            "zed"
          ];
        };

        networking = {
          knownNetworkServices = [ "Wi-Fi" ];
          dns = [
            "1.1.1.1"
          ];
        };
      };
    in
    {
      # Build darwin flake using:
      # $ darwin-rebuild build --flake .#Valentins-MacBook-Pro
      darwinConfigurations."Valentins-MacBook-Pro-2" = nix-darwin.lib.darwinSystem {
        modules = [
          { nix.enable = false; }
          configuration
          home-manager.darwinModules.home-manager
          {
            home-manager.useGlobalPkgs = true;
            home-manager.useUserPackages = true;
            home-manager.users.valentinmouret = import ./home.nix;
          }
        ];
      };

      # Expose the package set, including overlays, for convenience.
      darwinPackages = self.darwinConfigurations."Valentins-MacBook-Pro".pkgs;
    };
}
