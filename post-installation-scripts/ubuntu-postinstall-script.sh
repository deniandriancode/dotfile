#!/bin/bash

# refresh apt repositories
function refreshRepo () {
    sudo apt update
}

# install a list of packages
function installPackage () {
    target=$1
    printf "Installing $target\n"
    sudo apt install --yes $target
    echo
}

# install brave browser
function braveBrowserInstall () {
    sudo apt install apt-transport-https curl
    sudo curl -fsSLo /usr/share/keyrings/brave-browser-archive-keyring.gpg https://brave-browser-apt-release.s3.brave.com/brave-browser-archive-keyring.gpg
    echo "deb [signed-by=/usr/share/keyrings/brave-browser-archive-keyring.gpg arch=amd64] https://brave-browser-apt-release.s3.brave.com/ stable main"|sudo tee /etc/apt/sources.list.d/brave-browser-release.list
    sudo apt update
    sudo apt install brave-browser
}

# install nix package manager
# this will install nix for multiuser, remember to disable selinux to be able
# authenticate with sudo
function nixInstall () {
    sh <(curl -L https://nixos.org/nix/install) --daemon
}

# xmonad installation
function xmonadInstall () {
    installPackage xmonad
    installPackage libghc-xmonad*
    installPackage xmobar
    rm -rf ~/.xmonad
    rm -rf ~/.xmobar
    mkdir ~/.xmonad
    mkdir ~/.xmobar
    cp ../xmonad.hs ~/.xmonad
    cp ../xmobarrc ~/.xmobar
    xmonad --recompile
    xmobar --restart
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

# packages for development
development=(
    emacs
    git
    vim
    neovim
    suckless-tools
    openjdk-17-*
    build-essential
)

# media, only install after adding packman to repository
media=(
    gimp
    inkscape
    vlc
    flameshot
)

# comment and uncomment lines below
# executing commands
refreshRepo
for item in ${development[@]} ${development[@]} ${media[@]}
do
    installPackage $item
done
braveBrowserInstall
nixInstall
doomEmacsInstall
xmonadInstall

