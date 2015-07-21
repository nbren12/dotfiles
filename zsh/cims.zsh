export DOTFILES=$HOME/.dotfiles
source $DOTFILES/zsh/common.zsh


export LD_LIBRARY_PATH=$HOME/usr/lib:$LD_LIBRARY_PATH
export PATH=$HOME/usr/bin:$HOME/usr/local/bin:/scratch/noah/linux_xorg7_64:$PATH

export CC=gcc
export CXX=g++
export PATH=/usr/local/texlive/2013/bin/x86_64-linux:$PATH

# Setup fortran compiler
export FC=ifort
source /opt/intel/composerxe/bin/compilervars.sh intel64

module load clang-3.5.0
module load gcc-4.8.2
module load qt-4.8.5


# Julia
module load gcc-4.8.2
export PATH=/usr/local/pkg/julia/0.3.0/bin:$PATH


#PYTHONPATH
export PATH=/scratch/noah/anaconda3/bin:$PATH
export PYTHONPATH=$HOME/gnl:$HOME/workspace/skeleton/python:$PYTHONPATH
export PYTHONPATH=$HOME/workspace/skelfilter:$PYTHONPATH
alias ipynb='ipython notebook --pdb --port=8889 --deep-reload --no-browser'

# Git annex
export PATH=$HOME/usr/git-annex.linux:$PATH

# Emacs
module load emacs-24.4

# LYx
module load  lyx-2.1.3

# VIM
module load vim-7.4

umask 2
