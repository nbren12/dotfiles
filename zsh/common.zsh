#
# Get CIMS defaults
#
if [ -r /usr/local/lib/.system.kshrc ] ; then
    . /usr/local/lib/.system.kshrc
    export PYTHONPATH=""
fi


# Path to your oh-my-zsh configuration.
ZSH=$HOME/.dotfiles/zsh/oh-my-zsh

# Set name of the theme to load.
# Look in ~/.oh-my-zsh/themes/
# Optionally, if you set this to "random", it'll load a random theme each
# time that oh-my-zsh is loaded.
ZSH_THEME="bira"
# ZSH_THEME="darkblood"
# ZSH_THEME="ys"

# Example aliases
# alias zshconfig="mate ~/.zshrc"
# alias ohmyzsh="mate ~/.oh-my-zsh"

# Set to this to use case-sensitive completion
# CASE_SENSITIVE="true"

# Comment this out to disable bi-weekly auto-update checks
# DISABLE_AUTO_UPDATE="true"

# Uncomment to change how often before auto-updates occur? (in days)
# export UPDATE_ZSH_DAYS=13

# Uncomment following line if you want to disable colors in ls
# DISABLE_LS_COLORS="true"

# Uncomment following line if you want to disable autosetting terminal title.
# DISABLE_AUTO_TITLE="true"

# Uncomment following line if you want to disable command autocorrection
# DISABLE_CORRECTION="true"

# Uncomment following line if you want red dots to be displayed while waiting for completion
# COMPLETION_WAITING_DOTS="true"

# Uncomment following line if you want to disable marking untracked files under
# VCS as dirty. This makes repository status check for large repositories much,
# much faster.
DISABLE_UNTRACKED_FILES_DIRTY="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Custom plugins may be added to ~/.oh-my-zsh/custom/plugins/
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(git)

source $ZSH/oh-my-zsh.sh

# Customize to your needs...
# ZSH options
unsetopt autopushd
zmodload zsh/mathfunc


__git_files () { 
    _wanted files expl 'local files' _files     
}


###########################################################################
#                         Madrespek Modifications                         #
###########################################################################

export DOTFILES="$HOME/.dotfiles"
export PYTHONPATH=${DOTFILES}/python:$PYTHONPATH

#   Ipython completion
source $DOTFILES/ipython-completion.bash

#######USER ALIASES########################

alias prox='ssh -YD 9999 ndb245@access.cims.nyu.edu'
alias webd="cd ~/Sites/"
alias ssh='ssh -Y'
alias mnt_helix='sshfs -o transform_symlinks,follow_symlinks helix: /Users/noah/shares/helix'
alias aria='aria2c -d $HOME/Desktop -j 4 -x 4 --load-cookies=$HOME/.cookies -Z'
alias fserve='aria2c -d $HOME/Downloads/Warez -j 4 -x 4 --load-cookies=$HOME/.cookies -Z'
alias ipy='ipython qtconsole --pylab=inline'

alias screen='screen -R'
alias p='pushd'
alias o='popd' 
alias d='dirs -v'

alias lmk='latexmk -pdf -pvc'
alias ipynb='ipython notebook --browser=no'
alias pyhttp='python -m SimpleHTTPServer'
alias ml='matlab -nodesktop -nosplash'
alias ijulia='ipython --profile=julia'
alias ijulianb='ipython notebook --profile=julia'

alias vi="vim -u NONE"
if [[ $(uname) == "Darwin" ]] 
then

else
    alias g='gvim --servername VIM1 --remote-silent'
    alias mlab='matlab -nodisplay'
    alias vim='vim -X'
fi
    



###########################################################################
#                         Environmental Variables                         #
###########################################################################

export PATH=/usr/local/texlive/2013/bin/x86_64-linux/:$PATH

export EDITOR='/usr/bin/env vim'

export DOTFILES=$HOME/.dotfiles
export PATH=$DOTFILES/bin:$PATH

export ETS_TOOLKIT=qt4



