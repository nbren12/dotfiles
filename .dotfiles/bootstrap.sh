#!/bin/sh

git clone --separate-git-dir=$HOME/.myconf git@github.com:nbren12/dotfiles.git $HOME/myconf-tmp
rsync -av ~/myconf-tmp/ $HOME/
rm -rf ~/myconf-tmp/
# alias config='/usr/bin/git --git-dir=$HOME/.myconf/ --work-tree=$HOME'
