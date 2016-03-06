# User specific aliases and functions
alias ssh='ssh -Y'
alias qs='qstat -u ndb245'
alias qb='qsub'
alias interactive_session='qsub -I -X -q interactive -l nodes=1:ppn=12,walltime=04:00:00'

# alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias vim='vim -X'
alias ml='matlab -nodesktop -nosplash'
alias rs='rsync -aHv'

alias emacs='emacs'
alias ed='emacs --daemon'
alias ec='emacsclient'
alias ecw='emacsclient -nw'

alias screen='screen -R'
alias p='pushd'
alias o='popd' 
alias d='dirs -v'
