export DOTFILES=$HOME/.dotfiles

source $DOTFILES/zsh/common.zsh

export LD_LIBRARY_PATH=$HOME/usr/lib:$LD_LIBRARY_PATH
export PATH=$HOME/usr/bin:$HOME/usr/local/bin:/scratch/noah/linux_xorg7_64:$PATH
export PYTHONPATH=/home/noah/Sync/tropical_modeling:/kontiki_array1/noah/rayben:$PYTHONPATH


export CC=gcc
export CXX=g++
export PATH=/home/noah/anaconda/bin:/usr/local/texlive/2013/bin/x86_64-linux:/kontiki_array1/noah/rayben/rayben/bin:$HOME/usr/local/git-annex.linux:$PATH
export PATH=/home/noah/array/linux_xorg7_64:$PATH
source activate aos

alias tropnb='emacs-24.3 /home/noah/Sync/tropical_modeling/notebook/notebook.org'



#VIRTUAL_ENV_DISABLE_PROMPT=1 source /home/noah/usr/python-env/bin/activate
umask 2

