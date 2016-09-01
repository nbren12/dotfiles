
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

alias em='emacsclient --alternate-editor="" $args'


# shortcut for ncview
function nv {
        command ncview "$@" &
}



# Function for ipytohn notebook
ipynb ()
{

    #!/bin/sh
    #PBS -l nodes=1,ppn=8,walltime=4:00:00
    
    source $HOME/py3k/bin/activate
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

# Swap files
function sw(){
mv "$2" .tmp123124125
mv "$1" "$2"
mv .tmp123124125 "$1"

}

# Open manual in preview
pman() 
{ 
    man -t "$@" | open -f -a Preview; 
}

# name of file
na ()
{

    if [  "$1" != "" ]; then
        name=$(readlink -f $1)
    else
        name=$(pwd)
    fi

    echo $name
    echo /ssh:mercer:$name
    echo scp mercer:$name 
}

# FZF stuff

fzf_cd ()
{
    # file=$(fzf)
    # [[ -f $file ]] && goto=$(dirname $file)
    # [[ -d $file ]] && goto=$file
    goto=$(find . -type d | grep -v '.git' | fzf)

    cd $goto

}

alias cf=fzf_cd

function fd ()
{
    cd $(find . -maxdepth 3 -not -path '*/\.*' -type d | fzf )
}

function projectroot ()
{
    markers=(.git .top)
    d=`pwd`
    gdir="/"

    while [[ ! $d == "/" ]]
    do
        for marker in $markers; do
            if [[ -e $d/$marker ]]; then
                gdir=$d
            fi
        done
        d=$(dirname $d)
    done
    echo $gdir
}

function findignore()
{
    find $1 -not -path '*\.git/*'
}

alias lp='findignore `projectroot` | fzf'
alias lpv='vim $(lp)'

# HPC aliases
alias qs='qstat -u ndb245'
alias wqs='watch -n 1 qstat -u ndb245'

function sst ()
{
    job=$(qstat -u ndb245 | tail -n 1 | awk -F' ' '{print $1}')
    showstart  $job
}
alias wst='watch -n 5 sst'
alias interactive_session='qsub -I -X -q interactive -l nodes=1:ppn=8,walltime=04:00:00'
alias is='interactive_session'

# Docker aliases
alias docker_env='eval $(docker-machine env default)'

# User specific aliases and functions
alias ssh='ssh -Y'

# cmake alias
alias rmcmake='rm -rf CMakeFiles CMakeCache.txt'

# alias rm='rm -i'
alias cp='cp -i'
alias mv='mv -i'
alias vim='vim -X'
alias ml='matlab -nodesktop -nosplash'
alias rs='rsync -aHv'

alias ed='emacs --daemon'
alias ec='emacsclient'
alias ecc='emacsclient -c'
alias ecw='emacsclient -nw'

alias screen='screen -R'


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
alias mkdir_date='mkdir $(date +%F)'
