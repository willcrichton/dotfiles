#!/bin/bash

sudo apt-get -y update
sudo apt-get -y install zsh git emacs24
sudo chsh $USER -s /bin/zsh
