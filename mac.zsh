export DOTFILES=$HOME/.dotfiles

source $DOTFILES/zsh/common.zsh


alias g='mvim --servername VIM1 --remote-silent'
alias mlab='/Applications/MATLAB_R2013a.app/bin/matlab -nodesktop -nosplash'
alias vim='mvim -v'
alias marked="open -a Marked.app"
alias un_cims="unison -batch cims"
alias un_mm="unison -batch macmini"
alias tropnb="emacs /Users/noah/Sync/tropical_modeling/notebook/notebook.org"




export UDUNITS2_XML_PATH=/anaconda/envs/aos/share/udunits/udunits2.xml

export PATH=/usr/local/bin:/Users/noah/local/bin:/Users/noah/usr/bin:/Users/noah/.cabal/bin:/usr/local/texlive/2010/bin/x86_64-darwin:/Users/noah/bin:/usr/local/share/python:/usr/bin:/bin:/usr/sbin:/sbin:/usr/local/bin:/opt/X11/bin:$PATH
export PATH=/Applications/git-annex.app/Contents/MacOS:$PATH
export PATH=/usr/local/pkg/emacs/24.3/bin:$PATH

export PKG_CONFIG_PATH=/opt/X11/lib/pkgconfig:/usr/local/Library/ENV/pkgconfig/10.8:$PKG_CONFIG_PATH
export PYTHONPATH=$DOTFILES/python:$PYTHONPATH



export PATH=/usr/local/bin:/anaconda/bin:$PATH

export NCARG_ROOT=/opt/ncl-6.2.0
export PATH=$NCARG_ROOT/bin:$PATH

# VIRTUAL_ENV_DISABLE_PROMPT=1 source /Users/noah/Library/Enthought/Canopy_64bit/User/bin/activate

# Setting PATH for EPD-7.3-2
# The orginal version is saved in .bash_profile.pysave
export PATH
export PYTHONPATH=/Users/noah/proj/rayben:$PYTHONPATH


source ~/Sync/tropical_modeling/activate
source activate aos
alias pynb='cd ~/Dropbox/ipython-notebooks && tmux new "ipython notebook"'
alias emacs='/Applications/Emacs.app/Contents/MacOS/Emacs'

MKL_NUM_THREADS=1
export MKL_NUM_THREADS
export PATH=/opt/local/bin:$PATH
