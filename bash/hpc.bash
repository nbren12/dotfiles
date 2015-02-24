# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Load user aliases and funs
source ~/.dotfiles/bash/common.sh
source ~/.dotfiles/bash/alias.sh
source ~/.dotfiles/bash/funs.sh

# load modules
#source /etc/profile.d/env-modules.sh
module load mvapich2

# the nco utilities
export PATH=$PATH:/share/apps/netcdf/4.1.1/intel/serial/nco-4.0.1/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/share/apps/netcdf/4.1.1/intel/serial/nco-4.0.1/lib/

# netcdf
export PATH=$PATH:/share/apps/netcdf/4.1.1/intel/serial/netcdf/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/share/apps/hdf5/1.8.4/intel/serial/lib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/share/apps/netcdf/4.1.1/intel/serial/netcdf/lib

# netcdf tools
module load ncview
module load cdo
module load nco

module load ncl
export NCARG_ROOT=/share/apps/ncl/6.2.0/lib/ncarg/

# matlab
module load matlab

module unload udunits
# module load iris

module load git

# Vim
module load vim/gnu/7.4

# Emacs
module load emacs/gnu/24.3

# Set user path and pythonpath
export TERM="xterm-256color"
export PATH=$HOME/usr/bin:$PATH


# Python
source  ~/py3k/bin/activate
export PYTHONPATH=$HOME/gnl:$PYTHONPATH

# This make everything readable.  This means that your files are not private, 
# allowing other people to share your data and output, which aids in research
# and enables me to help you if you have a problem.  
# As a general rule, you should not put personal information on a public 
# system like this.  

umask u=rwx,g=rx,o=rx
