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
alias ee='emacsclient -c -a vim'
# alias vim='emacsclient -nw -a vim'

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
alias gst='git status'

alias download_ycmd_conf='wget https://raw.githubusercontent.com/Valloric/ycmd/master/cpp/ycm/.ycm_extra_conf.py'

# configuration management
# from https://news.ycombinator.com/item?id=11070797
# Initialize uinsg  git clone --separate-git-dir=~/.myconf /path/to/repo ~
alias config='/usr/bin/env git --git-dir=$HOME/.myconf/ --work-tree=$HOME'
alias con='config'

# run mpi cleanly on olympus
# thanks to peter blossey
alias mpirun_clean='mpirun  -mca btl tcp,self'

alias aa='activate_above'
alias ipy='ipython'

alias ls='ls -F'

# google cloud
alias mysite='gcloud compute ssh nbren12@ubuntu'

# weather
alias weather='curl wttr.in'

alias skim='open -a Skim'
alias edit_aliases='vim ~/.dotfiles/shell/aliases.sh'

# jupyter
alias jls='jupyter notebook list'
alias jlab='jupyter lab --no-browser --port $JUPYTER_PORT'
alias nbc='jupyter nbconvert'
alias qtconsole='jupyter qtconsole'
alias notebook='jupyter notebook'

# GCS vms
alias vmstart='gcloud compute instances start --zone us-central1-a noah-vm'
alias vmstop='gcloud compute instances stop --zone us-central1-a noah-vm'
alias vmssh='gcloud compute ssh --zone us-central1-a noahb@noah-vm'
alias vmls='gcloud compute instances list'

# interpret color codes with less
alias less='less -r'

# kubectl
alias k='kubectl'
