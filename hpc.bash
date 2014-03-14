# .bashrc

# User specific aliases and functions

# check the queue and submit things faster
alias qs='qstat -u nyuID'
alias qb='qsub'

alias interactive_session='qsub -I -X -q interactive -l nodes=1:ppn=8,walltime=04:00:00'

# some nice terminals
function t1 {
        command xterm -sb -sl 1000 -fa Monaco -fs 11 -bg lemonchiffon3 &
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
alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'

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
source /etc/profile.d/env-modules.sh
module load mvapich2/intel/1.7

# the nco utilities
export PATH=$PATH:/share/apps/netcdf/4.1.1/intel/serial/nco-4.0.1/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/share/apps/netcdf/4.1.1/intel/serial/nco-4.0.1/lib/

# netcdf
export PATH=$PATH:/share/apps/netcdf/4.1.1/intel/serial/netcdf/bin
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/share/apps/hdf5/1.8.4/intel/serial/lib
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:/share/apps/netcdf/4.1.1/intel/serial/netcdf/lib

# ncview
module load udunits/intel/2.1.19
module load ncview/intel/2.0beta4 

# matlab
module load matlab/R2011a


# User Paths
export PATH=/home/ndb245/usr/bin:$PATH

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




