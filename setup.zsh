#!/bin/zsh

foreach file (emacs vimrc Rprofile gitconfig vimrc vim emacs.d 
   zsh/zshrc zsh/cims.zsh unison )
{
    DEST=~/.${file:t}
    if [[ ! -e  $DEST ]]
    then
        ln -s ~/.dotfiles/$file $DEST
    fi

}
