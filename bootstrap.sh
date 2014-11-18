pushd ~/.dotfiles

# Setup vim
ln -s ~/.dotfiles/vim ~/.vim
cp vimrc ~/.vimrc
curl https://raw.githubusercontent.com/Shougo/neobundle.vim/master/bin/install.sh | sh
