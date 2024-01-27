{
  description = "Example Darwin system flake";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    nix-darwin.url = "github:LnL7/nix-darwin";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgs";

    # Home-manager stuff
    home-manager.url = "github:nix-community/home-manager";
    home-manager.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = inputs@{ self, nix-darwin, nixpkgs, home-manager, ... }:
  let
    configuration = { pkgs, ... }: {
      # List packages installed in system profile. To search by name, run:
      # $ nix-env -qaP | grep wget
      environment.systemPackages = with pkgs;
      [
        delta # replacement for diff
        babashka
        clojure
        direnv
        emacs
        fd
        fzf
        git
        eza
        nodejs
        ripgrep
      ];
      environment.shellAliases = {
        ls = "eza";
        ll = "ls -l";
        grep = "rg";

        gs = "git status";
        ga = "git add";
        gl = "git log";
      };
      environment.variables = {
        EDITOR = "emacs";
      };

      fonts.fontDir.enable = true;
      fonts.fonts = with pkgs; [
        iosevka
      ];

      # Auto upgrade nix package and the daemon service.
      services.nix-daemon.enable = true;
      # nix.package = pkgs.nix;

      # Necessary for using flakes on this system.
      nix.settings.experimental-features = "nix-command flakes";

      # Create /etc/zshrc that loads the nix-darwin environment.
      programs.zsh = {
        enable = true;  # default shell on catalina
        enableFzfCompletion = true;
        enableFzfGit = true;
        enableFzfHistory = true;
        enableSyntaxHighlighting = true;
      };

      programs.fish = {
        enable = true;
      };

      services.postgresql = {
        enable = true;
        # ensureUsers = [
        #   {
        #     name = "postgres";
        #     ensurePermissions = {
        #       "ALL TABLES IN SCHEMA public" = "ALL PRIVILEGES";
        #     };
        #   }
        #   {
        #     name = "valentinmouret";
        #     ensurePermissions = {
        #       "ALL TABLES IN SCHEMA public" = "ALL PRIVILEGES";
        #     };
        #   }
        # ];
      };
      # programs.fish.enable = true;

      # Set Git commit hash for darwin-version.
      system.configurationRevision = self.rev or self.dirtyRev or null;

      # Used for backwards compatibility, please read the changelog before changing.
      # $ darwin-rebuild changelog
      system.stateVersion = 4;

      system.defaults.".GlobalPreferences"."com.apple.mouse.scaling" = 1.0;
      system.defaults.NSGlobalDomain.AppleInterfaceStyleSwitchesAutomatically = true;
      system.defaults.NSGlobalDomain.InitialKeyRepeat = 0;
      system.defaults.NSGlobalDomain.NSAutomaticCapitalizationEnabled = false;
      system.defaults.NSGlobalDomain.NSAutomaticSpellingCorrectionEnabled = false;
      system.defaults.NSGlobalDomain."com.apple.mouse.tapBehavior" = 1;
      system.defaults.NSGlobalDomain."com.apple.trackpad.scaling" = 3.0;
      system.defaults.dock.autohide = true;
      system.defaults.trackpad.Clicking = true;
      system.keyboard.enableKeyMapping = true;
      system.keyboard.remapCapsLockToControl = true;
      
      # The platform the configuration will be used on.
      nixpkgs.hostPlatform = "aarch64-darwin";

      # Necessary for home-manager to work
      users.users.valentinmouret.home = "/Users/valentinmouret";

      homebrew = {
        enable = true;
        casks = [
          "iterm2"
          "cyberduck"
          "dozer"
          "google-chrome"
          "hey"
          "logseq"
          "postico"
          "signal"
          "slack"
          "vlc"
          "whatsapp"
        ];
      };

      networking = {
        knownNetworkServices = ["Wi-Fi"];
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
