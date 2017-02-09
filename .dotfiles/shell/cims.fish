
# User specific aliases and functions
alias ssh='ssh -Y'

# cmake alias
alias rmcmake='rm -rf CMakeFiles CMakeCache.txt'

alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias ml='matlab -nodesktop -nosplash'

alias ed='emacs --daemon'
alias ec='emacsclient'
alias ecc='emacsclient -c'
alias ecw='emacsclient -nw'
alias ee='emacsclient -c -a emacs'
#alias vim='emacsclient -nw -a vim'

alias screen='screen -R'


# Process management
alias pss='ps aux | fzf'

# Directory movement
alias p='pushd'
alias o='popd'
alias d='dirs -v'
alias ..='cd ..'
alias ...='cd ../..'

alias lmk='latexmk -pdf'
alias pyhttp='python -m SimpleHTTPServer'
alias ml='matlab -nodesktop -nosplash'


alias g='git'
alias gls='git status -s | less'
alias ga='git commit --amend'

alias mkdir_date='mkdir (date +%F)'

# configuration management
# from https://news.ycombinator.com/item?id=11070797
alias config='/usr/bin/env git --git-dir=$HOME/.myconf/ --work-tree=$HOME'
alias con='config'
alias qtconsole='jupyter qtconsole'
alias notebook='jupyter notebook'

alias aa='activate_above'



# fzf commands
alias cf 'cd (fzf)'
# environment on cims
set -x PATH /home/noah/bin /home/noah/usr/bin /scratch/noah/anaconda3/bin $PATH

# Load fishmarks (http://github.com/techwizrd/fishmarks)
. $HOME/.fishmarks/marks.fish
