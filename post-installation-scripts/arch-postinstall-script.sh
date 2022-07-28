#!/bin/bash

# refresh apt repositories
function refreshRepo () {
    sudo pacman -Sy
}

function upgradeSystem () {
    sudo pacman -Syyu
}

function installPackage () {
    if pacman -Qi $1 &> /dev/null; then
		tput setaf 2
  		echo "###############################################################################"
  		echo "################## The package "$1" is already installed"
      	echo "###############################################################################"
      	echo
		tput sgr0
	else
    	tput setaf 3
    	echo "###############################################################################"
    	echo "##################  Installing package "  $1
    	echo "###############################################################################"
    	echo
    	tput sgr0
    	sudo pacman -S --noconfirm --needed $1
    fi
}

# install brave browser
function braveInstall () {
    cd Download
    git clone https://aur.archlinux.org/brave-bin.git
    cd brave-bin
    makepkg -si
}

# install nix package manager
# this will install nix for multiuser, remember to disable selinux to be able
# authenticate with sudo
function nixInstall () {
    sh <(curl -L https://nixos.org/nix/install) --daemon
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

function xmonadInstall () {
    rm -rf ~/.xmonad
    rm -rf ~/.xmobar
    mkdir ~/.xmonad
    mkdir ~/.xmobar
    cp ../xmonad.hs ~/.xmonad
    cp ../xmobarrc ~/.xmobar
    xmonad --recompile
    xmobar --restart
}

# it will also install xmonad
base=(
sddm
thunar
thunar-archive-plugin
thunar-volman
xfce4-terminal
xmonad
xmobar
xmonad-contrib
haskell-dbus
xmonad-utils
xmonad-log
nitrogen
dmenu
ark
)

development=(
firefox
flameshot
meld
simplescreenrecorder
scrot
base-devel
git
vim
neovim
emacs
jdk17-openjdk
wget
neofetch
htop
curl
)

sound=(
pulseaudio
pulseaudio-alsa
pavucontrol
alsa-firmware
alsa-lib
alsa-plugins
alsa-utils
gstreamer
gst-plugins-good
gst-plugins-bad
gst-plugins-base
gst-plugins-ugly
playerctl
volumeicon
)

bluetooth=(
pulseaudio-bluetooth
bluez
bluez-libs
bluez-utils
blueberry
)

utils=(
unace
unrar
zip
unzip
sharutils
uudeview
arj
cabextract
file-roller
dconf-editor
arc-gtk-theme
lxappearance
qt5ct
inxi
mtpfs
gvfs-mtp
gvfs-gphoto2
)

media=(
gimp
inkscape
vlc
)

font=(
awesome-terminal-fonts
adobe-source-sans-fonts
cantarell-fonts
noto-fonts
ttf-bitstream-vera
ttf-dejavu
ttf-droid
ttf-hack
ttf-inconsolata
ttf-liberation
ttf-roboto
ttf-ubuntu-font-family
tamsyn-font
)

# do not change the order, you may comment or uncomment lines
refreshRepo
upgradeSystem   # you maybe have to reboot after upgrading your system
for item in "${base[@]} ${development[@]} ${sound[@]} ${bluetooth[@]} ${utils[@]} ${media[@]} ${font[@]}" ; do
    installPackage $item
done
braveInstall
xmonadInstall
nixInstall
doomEmacsInstall
javaDevInstall
