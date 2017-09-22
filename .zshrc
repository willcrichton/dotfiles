## Prezto
if [[ -s "${ZDOTDIR:-$HOME}/.zprezto/init.zsh" ]]; then
    source "${ZDOTDIR:-$HOME}/.zprezto/init.zsh"
fi


## Emacs
export EDITOR="emacs" # for commands like crontab that use EDITOR
alias e='emacsclient -nw'
alias emacs=e
# This forces Emacs to run emacs --daemon on first usage of emacsclient
export ALTERNATE_EDITOR=''


## Misc exports/aliases
export TERM="xterm-256color" # use all 256 colors
alias m='make -j'
alias dc=docker-compose
alias nd=nvidia-docker
alias ndc=nvidia-docker-compose
alias kc=kubectl
