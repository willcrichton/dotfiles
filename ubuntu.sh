#!/bin/bash

sudo add-apt-repository -y ppa:kelleyk/emacs
sudo apt-get -y update
sudo apt-get -y install zsh git emacs25 gdb htop
sudo chsh $USER -s /bin/zsh
