export LD_LIBRARY_PATH=$HOME/usr/lib:$LD_LIBRARY_PATH
export PATH=$HOME/usr/bin:$HOME/usr/local/bin:$PATH
export PYTHONPATH=/kontiki_array1/noah/rayben:$PYTHONPATH


export CC=gcc
export CXX=g++
export PATH=/usr/local/texlive/2013/bin/x86_64-linux:/kontiki_array1/noah/rayben/rayben/bin:$HOME/usr/local/git-annex.linux:$PATH

module load python-2.7
module load qt

VIRTUAL_ENV_DISABLE_PROMPT=1 source /home/noah/usr/python-env/bin/activate
umask 2

