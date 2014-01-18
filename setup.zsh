#!/bin/zsh

foreach file (emacs vimrc Rprofile gitconfig vimrc vim emacs.d 
   zsh/zshrc zsh/cims.zsh )
{
    ln -s ~/.dotfiles/$file ~/.${file:t}

}
