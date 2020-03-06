#!/bin/sh


if [[ ! -d  $HOME/.myconf ]] 
then
git clone --separate-git-dir=$HOME/.myconf git@github.com:nbren12/dotfiles.git $HOME/myconf-tmp
rsync -av ~/myconf-tmp/ $HOME/
rm -rf ~/myconf-tmp/
fi

echo "Add the following alias to your configuration"
echo alias config='/usr/bin/git --git-dir=$HOME/.myconf/ --work-tree=$HOME'
