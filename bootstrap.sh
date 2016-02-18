#!/bin/bash
# A script useful for bootstrapping my configurations on a new
# computer

if ! [ -e ~/.dotfiles/ ]
then
    git clone https://github.com/nbren12/dotfiles.git ~/.dotfiles
fi

pushd ~/.dotfiles

# Setup git
ln -s ~/.dotfiles/gitconfig ~/.gitconfig

# Setup vim
if ! [ -e ~/.vim ]; then
    ln -s ~/.dotfiles/vim ~/.vim
    cp vimrc ~/.vimrc
fi

# Setup emacs
if ! [ -e ~/.emacs.d ]
then
    mv ~/.emacs.d ~/.emacs.d.bak

    # git clone --recursive http://github.com/syl20bnr/spacemacs ~/.emacs.d
    # ln -s ~/.dotfiles/.spacemacs ~/.spacemacs
    # ln -s ~/.dotfiles/spacemacs-private/* ~/.emacs.d/private/

    ln -s ~/.dotfiles/emacs.d ~/.emacs.d
    pushd ~/.emacs.d
    git clone https://github.com/AndreaCrotti/yasnippet-snippets.git snippets
    popd
fi

# Setup mr
pushd ~/usr/bin
curl https://raw.githubusercontent.com/joeyh/myrepos/master/mr > mr
chmod 755 mr
popd

# Setup zsh
if ! [ -e ~/.zprezto ]; then
    git clone --recursive https://github.com/sorin-ionescu/prezto.git "${ZDOTDIR:-$HOME}/.zprezto"

    # move config files to home dir
    setopt EXTENDED_GLOB
    for rcfile in "${ZDOTDIR:-$HOME}"/.zprezto/runcoms/^README.md(.N); do
        ln -s "$rcfile" "${ZDOTDIR:-$HOME}/.${rcfile:t}"
    done
fi

