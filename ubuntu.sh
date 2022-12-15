#!/bin/bash

sudo add-apt-repository -y ppa:kelleyk/emacs
sudo apt-get -y update
sudo apt-get -y install zsh git emacs28 gdb htop python3-venv build-essential
sudo chsh $USER -s /bin/zsh
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
