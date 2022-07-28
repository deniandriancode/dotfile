#!/bin/bash

# refresh zypper repositories
function refreshRepo () {
    sudo zypper refresh
}

# adding packman repo for leap 15.4
function addPackmanRepo () {
    refreshRepo
    sudo zypper ar -cfp 90 'https://ftp.gwdg.de/pub/linux/misc/packman/suse/openSUSE_Leap_$releasever/' packman
    sudo zypper dup --from packman --allow-vendor-change
}

# install a list of packages
function installPackage () {
    target=$1
    printf "Installing $target\n"
    sudo zypper install --no-confirm -l --auto-agree-with-product-licenses $target
    echo
}

# install brave browser
function braveBrowserInstall () {
    sudo zypper install curl
    sudo rpm --import https://brave-browser-rpm-release.s3.brave.com/brave-core.asc
    sudo zypper addrepo --refresh https://brave-browser-rpm-release.s3.brave.com/x86_64/ brave-browser
    sudo zypper install brave-browser
}

# install nix package manager
# this will install nix for multiuser, remember to disable selinux to be able
# authenticate with sudo
function nixInstall () {
    sh <(curl -L https://nixos.org/nix/install) --daemon
}

# xmonad installation
function xmonadInstall () {
    installPackage xmonad*
    installPackage ghc-xmonad*
    installPackage xmobar
    installPackage ghc-xmobar*
    rm -rf ~/.xmonad
    rm -rf ~/.xmobar
    mkdir ~/.xmonad
    mkdir ~/.xmobar
    cp ../xmonad.hs ~/.xmonad
    cp ../xmobarrc ~/.xmobar
    xmonad --recompile
    xmobar --restart
}

# installation for java development (maven and gradle)
function javaDevInstall () {
    wget https://dlcdn.apache.org/maven/maven-3/3.8.6/binaries/apache-maven-3.8.6-bin.tar.gz
    tar -xvf apache-maven-3.8.6-bin.tar.gz
    mv apache-maven-3.8.6-bin.tar.gz ~/.local/share
    echo "export PATH=$PATH:/home/$USER/.local/share/apache-maven-3.8.6/bin" >> ~/.bashrc
    source ~/.bashrc
    curl -s "https://get.sdkman.io" | bash
    source "$HOME/.sdkman/bin/sdkman-init.sh"
    sdk install gradle
}

# build essential compatible
function buildEssentialInstall () {
    sudo zypper install -t pattern devel_basis
}

# install doom emacs, make sure you have installed git and emacs 27.1+ on your machine
# backup your ~/.emacs.d folder or ~/.emacs file
function doomEmacsInstall () {
    deps=(ripgrep find fd)
    for dep in ${deps[@]}
    do
        installPackage $dep
    done
    git clone --depth 1 https://github.com/doomemacs/doomemacs ~/.emacs.d
    ~/.emacs.d/bin/doom install
}

# basic codecs, only install these codecs after adding packman repository
codec=(
    ffmpeg
    gstreamer-plugins-good
    gstreamer-plugins-bad
    gstreamer-plugins-libav
    gstreamer-plugins-ugly
    libavcodec-full
    vlc-codecs
)

# packages for development
development=(
    emacs
    git
    vim
    neovim
    suckless-tools
    java-17-openjdk*
    nodejs16*
)

# media, only install after adding packman to repository
media=(
    gimp
    inkscape
    vlc
    audacity
    flameshot
)

# comment and uncomment lines below
# executing commands
refreshRepo
addPackmanRepo
for item in ${development[@]} ${codec[@]} ${development[@]} ${media[@]}
do
    installPackage $item
done
braveBrowserInstall
nixInstall
doomEmacsInstall
xmonadInstall

