alias lp='findignore `projectroot` | fzf'

# HPC aliases
alias qs='qstat -u ndb245'
alias wqs='watch -n 1 qstat -u ndb245'


alias em='emacsclient --alternate-editor="" $args'

alias wst='watch -n 5 sst'
alias interactive_session='qsub -I -X -q interactive -l nodes=1:ppn=8,walltime=04:00:00'
alias is='interactive_session'


# User specific aliases and functions
alias ssh='ssh -Y'

# cmake alias
alias rmcmake='rm -rf CMakeFiles CMakeCache.txt'

# alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias ml='matlab -nodesktop -nosplash'
alias rs='rsync -aHv'

alias ed='emacs --daemon'
alias ec='emacsclient'
alias ecc='emacsclient -c'
alias ew='emacsclient -nw'
alias ee='emacsclient -c -a emacs'
alias vim='emacsclient -nw -a vim'

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

alias download_ycmd_conf='wget https://raw.githubusercontent.com/Valloric/ycmd/master/cpp/ycm/.ycm_extra_conf.py'

# configuration management
# from https://news.ycombinator.com/item?id=11070797
alias config='/usr/bin/env git --git-dir=$HOME/.myconf/ --work-tree=$HOME'
alias con='config'
alias qtconsole='jupyter qtconsole'
alias notebook='jupyter notebook'

alias aa='activate_above'
