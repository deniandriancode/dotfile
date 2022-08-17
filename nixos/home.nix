{ config, pkgs, ... }:

{
  # Home Manager needs a bit of information about you and the
  # paths it should manage.
  home.username = "<user_name>";
  home.homeDirectory = "/home/<user_name>";

  home.packages = with pkgs; [
    htop
    lxappearance
    neofetch
    python310
    python310Packages.virtualenv
    xclip
    nodejs
    nodePackages.npm
    dart
    ghc
    stack
    cabal-install
    gcc
    conan
    gobject-introspection
    gnome.gnome-font-viewer
  ];

  home.sessionPath = [
    "$HOME/.local/bin"
  ];

  home.sessionVariables = {
    TMPDIR = ~/.tmp;
    EDITOR = "vim";
    TERMINAL = "xfce4-terminal";
    ANDROID_HOME = ~/Android;
    _JAVA_AWT_WM_NONREPARENTING = 1;
    AWT_TOOLKIT = "MToolkit";
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
    userName = "<git_username>";
    userEmail = "<git_email>";
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
