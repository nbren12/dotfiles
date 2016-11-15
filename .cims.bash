# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
  . /etc/bashrc
fi

# CIMS defaults
[[ -f /usr/local/lib/.system.bashrc ]] && . /usr/local/lib/.system.bashrc
export PYTHONPATH=""


# vapor
# . /scratch/noah/vapor-2.5.0/bin/vapor-setup.sh > /dev/null

# Python
export PATH=/scratch/noah/anaconda3/bin:$PATH

# 
export FC=gfortran

# Jetbrains IDES
export PATH=/scratch/noah/clion-2016.1/bin:/scratch/noah/pycharm-2016.1/bin:$PATH


# intel compilers
# module load intel64
# source  /opt/pkg/intel/2013/composer_xe_2013.1.117/bin/compilervars.sh intel64

# clang
# module load clang-3.9.0
# module load gcc-5.2.0

# git
export PATH=/scratch/noah/git-annex.linux:$PATH

# emacs
module load emacs-24.5
export PATH=/usr/local/stow/emacs-24.5/bin:/usr/local/pkg/emacs/24.5/bin:$PATH
 
export ALTERNATE_EDITOR=""
export EDITOR="emacsclient -t"                  # $EDITOR should open in terminal
export VISUAL="emacsclient -c -a emacs"         # $VISUAL opens in GUI with non-daemon as alternate


# lyx
module load lyx-2.2.0
export PATH=/usr/local/stow/lyx-2.2.0/bin:$PATH

# latex
module load texlive-2016
export PATH=/usr/local/stow/texlive-2016/bin/x86_64-linux:$PATH


# This make everything readable.  This means that your files are not private,
# allowing other people to share your data and output, which aids in research
# and enables me to help you if you have a problem.
# As a general rule, you should not put personal information on a public
# system like this.

function killhpc(){

    for i in `seq 1 4`
    do
        ssh login-0-$i killall $1
    done
}

# local files
export PATH=$HOME/usr/bin:$PATH

# Load user aliases and funs
source ~/.dotfiles/shell/bash/common.sh
source ~/.dotfiles/shell/bash/funalias.sh

module load texlive-2016 2> /dev/null

umask u=rwx,g=rx,o=rx
