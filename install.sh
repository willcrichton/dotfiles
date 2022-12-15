#!/bin/zsh

set -e

git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"
setopt EXTENDED_GLOB
for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
    ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
done

cp -r -f .emacs.d .gitignore .gitconfig .zshrc .zpreztorc .gdbinit $HOME

python3 -m venv $HOME/.base
git config --global core.excludesfile $HOME/.gitignore
