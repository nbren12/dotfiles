export DOTFILES=$HOME/.dotfiles

source $DOTFILES/zsh/common.zsh
export LD_LIBRARY_PATH=$HOME/usr/lib:$LD_LIBRARY_PATH
export PATH=$HOME/usr/bin:$HOME/usr/local/bin:/scratch/noah/linux_xorg7_64:$PATH
export PYTHONPATH=/home/noah/Sync/tropical_modeling:/kontiki_array1/noah/rayben:$PYTHONPATH

alias eclipse='/opt/pkg/eclipse/4.3.2/eclipse'

export CC=gcc
export CXX=g++
export PATH=/usr/local/texlive/2013/bin/x86_64-linux:/kontiki_array1/noah/rayben/rayben/bin:$PATH
export PATH=/home/noah/array/linux_xorg7_64:$PATH
# source activate aos

alias tropnb='emacs-24.3 /home/noah/Sync/tropical_modeling/notebook/notebook.org'

# Setup fortran compiler
export FC=ifort
source /opt/intel/composerxe/bin/compilervars.sh intel64

module load python-2.7
module load clang-3.5.0
module load gcc-4.8.2
VIRTUAL_ENV_DISABLE_PROMPT=1 source $HOME/pyenv/bin/activate

# Ruby
module load ruby-1.9.3
export PATH=$HOME/.gem/ruby/1.9.1/bin:$PATH

alias ipynb='ipython notebook --pdb --port=8889 --deep-reload --no-browser'

#VIRTUAL_ENV_DISABLE_PROMPT=1 source /home/noah/usr/python-env/bin/activate
umask 2

