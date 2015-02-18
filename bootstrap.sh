pushd ~/.dotfiles

# Setup vim
ln -s ~/.dotfiles/vim ~/.vim
cp vimrc ~/.vimrc
curl https://raw.githubusercontent.com/Shougo/neobundle.vim/master/bin/install.sh | sh

# Setup emacs
mv ~/.emacs.d ~/.emacs.d.bak

git clone --recursive http://github.com/syl20bnr/spacemacs ~/.emacs.d
ln -s ~/.dotfiles/.spacemacs ~/.spacemacs
ln -s ~/.dotfiles/spacemacs-private/* ~/.emacs.d/private/
