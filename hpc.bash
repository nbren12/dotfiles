# .bashrc

# User specific aliases and functions

# check the queue and submit things faster
alias qs='qstat -u ndb245'
alias qb='qsub'

alias interactive_session='qsub -I -X -q interactive -l nodes=1:ppn=8,walltime=04:00:00'

# some nice terminals
function t1 {
        command xterm -sb -sl 1000 -fa Monaco -fs 11  &
        }
function t2 {
        command xterm -sb -sl 1000 -fa Monaco -fs 11 -bg lightyellow3 &
        }
function t3 {
        command xterm -sb -sl 1000 -fa Monaco -fs 11 -bg antiquewhite3 &
        }
function t4 {
        command xterm -sb -sl 1000 -fa Monaco -fs 11 -bg LavenderBlush3 &
        }

# make sure you do not kill anything important!
# alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias vim='vim -X'

# shortcut to fire up emacs 
function em {
 command emacs "$@" &
}

# shortcut for ncview
function nv {
        command ncview "$@" &
        }

# This allows me to use matlab in the command line.
alias ml='matlab -nodesktop -nosplash'

# rsync is a better way of copying files
alias rs='rsync -aHv'


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

# Python

module load python/intel/2.7.6 

#VIM
module load gtkplus
#module load vim

export PYTHONPATH=/home/ndb245/.dotfiles/python:$PYTHONPATH
# User Paths
export PATH=$HOME/.dotfiles/bin:/home/ndb245/usr/bin:$PATH


# Function for ipytohn notebook
ipynb ()
{

    #!/bin/sh
    #PBS -l nodes=1,ppn=8,walltime=4:00:00
    
    source $HOME/pyenv/bin/activate
    PORT=$1
    
    ssh  -N -R  ${PORT}:127.0.0.1:${PORT} login-0-0 &
    PID1=$!
    ssh  -N -R  ${PORT}:127.0.0.1:${PORT} login-0-1 &
    PID2=$!
    ssh  -N -R  ${PORT}:127.0.0.1:${PORT} login-0-2 &
    PID3=$!
    ssh  -N -R  ${PORT}:127.0.0.1:${PORT} login-0-3 &
    PID4=$!

    ipython notebook --port=${PORT}
    echo "Killing procs"
    
    kill $PID1
    kill $PID2
    kill $PID3
    kill $PID4
    
}

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

umask u=rwx,g=rx,o=rx
# This make everything readable.  This means that your files are not private, 
# allowing other people to share your data and output, which aids in research
# and enables me to help you if you have a problem.  
# As a general rule, you should not put personal information on a public 
# system like this.  

module load vim/gnu/7.4


