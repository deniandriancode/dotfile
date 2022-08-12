{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "deni";
  home.homeDirectory = "/home/deni";

  home.packages = with pkgs; [
    htop
    lxappearance
    neofetch
    python310
    python310Packages.virtualenv
    nodejs
    nodePackages.npm
    ghc
    stack
    cabal-install
    gcc
    gobject-introspection
  ];

  home.sessionPath = [ "$HOME/.local/bin" ];

  home.sessionVariables = {
    TMPDIR = ~/.tmp;
  };

  programs.bash = {
    enable = true;
    shellAliases = {
      vi = "vim";
      iv = "vim";
    };
  };

  programs.git = {
    enable = true;
    userName = "deniandriancode";
    userEmail = "deniandriancode@gmail.com";
  };

  # This value determines the Home Manager release that your
  # configuration is compatible with. This helps avoid breakage
  # when a new Home Manager release introduces backwards
  # incompatible changes.
  #
  # You can update Home Manager without changing this value. See
  # the Home Manager release notes for a list of state version
  # changes in each release.
  home.stateVersion = "22.05";

  # Let Home Manager install and manage itself.
  programs.home-manager.enable = true;
}
