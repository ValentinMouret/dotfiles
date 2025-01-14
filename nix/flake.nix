{
  description = "Valentin’s nix darwin flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/24.05";

    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # Home-manager stuff
    home-manager.url = "github:nix-community/home-manager/release-24.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nix-darwin, home-manager, ... }:
    let
      configuration = { pkgs, ... }: {
        # List packages installed in system profile. To search by name, run:
        # $ nix-env -qaP | grep wget
        environment.systemPackages = with pkgs;
          [
            babashka
            bat
            bottom
            cargo
            clojure
            delve
            deno
            devenv
            direnv
            # CLI for building, runing, testing, and managing your Emacs Lisp dependencies
            # https://emacs-eask.github.io
            eask
            eza
            fd
            fzf
            git
            gitAndTools.gh
            go
            gopls
            gotools
            helix
            ispell
            iosevka
            jdt-language-server
            jetbrains-mono
            jq
            metals
            multimarkdown
            nerdfonts
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
            openjdk22
            poetry
            pyenv
            rage
            rust-analyzer
            rustc
            rustfmt
            ripgrep
            s3cmd
            sbcl
            sbt
            scala
            silver-searcher
            terraform-ls
            yarn
            zig
            zls
          ];
        environment.shellAliases = {
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
        services.nix-daemon.enable = true;
        # nix.package = pkgs.nix;

        # Necessary for using flakes on this system.
        nix.settings.experimental-features = "nix-command flakes";
        nix.optimise.automatic = true;

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
            "emacs-plus"
          ];
          taps = [
            "d12frosted/emacs-plus"
            "homebrew/services"
          ];
          casks = [
            "calibre"
            "claude"
            "cyberduck"
            "discord"
            "dozer"
            "figma"
            "firefox"
            "ghostty"
            "google-chrome"
            "hey"
            "iterm2"
            "linear-linear"
            "notion"
            "orbstack"
            "postico"
            "raycast"
            "rectangle"
            "roam-research"
            "signal"
            "slack"
            "vlc"
            "visual-studio-code"
            "wezterm"
            "whatsapp"
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
      darwinConfigurations."Valentins-MacBook-Pro" = nix-darwin.lib.darwinSystem {
        modules = [
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
