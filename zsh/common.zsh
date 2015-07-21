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
ZSH_THEME="cloud"
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

#######USER ALIASES########################

alias ssh='ssh -Y'

alias screen='screen -R'
alias p='pushd'
alias o='popd' 
alias d='dirs -v'

alias lmk='latexmk -pdf -pvc'
alias pyhttp='python -m SimpleHTTPServer'
alias ml='matlab -nodesktop -nosplash'

alias vi="vim -u NONE"
alias g='gvim --servername VIM1 --remote-silent'
alias vim='vim -X'

alias ed='emacs --daemon'
alias ec='emacsclient -c'

alias tmux='tmux -2' # For 256 colors

###########################################################################
#                         Environmental Variables                         #
###########################################################################

export DOTFILES=$HOME/.dotfiles
export PATH=$DOTFILES/bin:$PATH
