# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi


# Load user aliases and funs
source ~/.dotfiles/shell/bash/common.sh
source ~/.dotfiles/shell/bash/funalias.sh


export EMACS="/Applications/Emacs.app/Contents/MacOS/Emacs"
export EDITOR=$EMACS
alias emacs=$EMACS

# Set user path and pythonpath
export TERM="xterm-256color"
export PATH=$HOME/usr/bin:$PATH

export PYTHONPATH=$HOME/workspace/multicloudstochfmk13:$PYTHONPATH

# path
export PATH=/usr/local/bin:$PATH

# anaconda
export PATH=/Users/noah/anaconda3/bin:$PATH

# anaconda's fortran overrides system version
alias gfortran='/usr/local/bin/gfortran'
alias mpif90='/usr/local/bin/mpif90'
export FC='/usr/local/bin/gfortran'

# julia
export PATH=/Applications/Julia-0.5.app/Contents/Resources/julia/bin:$PATH

# vim
alias vim=nvim

# latex
export PATH=/usr/local/texlive/2014/bin/x86_64-darwin:$PATH

# CUDA
export CUDA_HOME=/usr/local/cuda
export DYLD_LIBRARY_PATH="$DYLD_LIBRARY_PATH:$CUDA_HOME/lib"
export PATH="$CUDA_HOME/bin:$PATH"

# This make everything readable.  This means that your files are not private, 
# allowing other people to share your data and output, which aids in research
# and enables me to help you if you have a problem.  
# As a general rule, you should not put personal information on a public 
# system like this.  
umask u=rwx,g=rx,o=rx
