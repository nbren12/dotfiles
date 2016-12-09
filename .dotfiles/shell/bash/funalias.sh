
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



# shortcut for ncview
function nv {
        command ncview "$@" &
}



# Function for ipython notebook

ipynb ()
{

    #!/bin/sh
    #PBS -l nodes=1,ppn=8,walltime=4:00:00

    source $HOME/py3k/bin/activate
    PORT=$1

    PID=""

    ssh  -N -R  ${PORT}:127.0.0.1:${PORT} login-0-0 &
    PID[0]=$!
    ssh  -N -R  ${PORT}:127.0.0.1:${PORT} login-0-1 &
    PID[1]=$!
    ssh  -N -R  ${PORT}:127.0.0.1:${PORT} login-0-2 &
    PID[2]=$!
    ssh  -N -R  ${PORT}:127.0.0.1:${PORT} login-0-3 &
    PID[3]=$!

    export JUPYTER_RUNTIME_DIR=$HOME/.local
    jupyter notebook --port=${PORT}

    echo "Killing procs"

    kill ${PID[*]}
}


dalma_notebook ()
{
    ssh dalma "/bin/bash -l -c 'jupyter-notebook --port=10002'"
}

#function for pbs submission of ipython 
pipynb ()
{
   subname=.submit.ipython.2393u
cat <<"EOF" > $subname
#!/bin/sh
#PBS -N ipython_notebook
#PBS -o ipy.out
#PBS -e ipy.err
#PBS -l nodes=1,walltime=04:00:00
PORT=10001
SSHPROC=

for i in `seq 0 3`
do
    ssh -N -R $PORT:127.0.0.1:$PORT login-0-$i &
    SSHPROC=$SSHPROJ $!
done

export JUPYTER_RUNTIME_DIR=$HOME/.local
jupyter notebook --port $PORT &
IPYPROC=$!
echo ""
echo "Running jupyter notebook on port $PORT"
echo ""
echo ""
echo ""
echo "Sleeping for 4 hours"
sleep 4h
echo "Killing proc"
kill $SSHPROC
kill $IPYPROC
EOF

qsub $subname
}

# function for slurm submission of ipython
sipynb ()
{

    subname=.submit.143325
cat <<"EOF" > $subname
#!/bin/sh
#SBATCH --ntasks=1
#SBATCH --job-name="jupyter-notebook"
#SBATCH --time=8:00:00
#SBATCH -o  .ipy.out
#SBATCH -e  .ipy.out
PORT=10002
SSHPROC=

for i in `seq 0 3`
do
    ssh -N -R $PORT:127.0.0.1:$PORT login-0-$i &
    SSHPROC=$SSHPROJ $!
done

export JUPYTER_RUNTIME_DIR=$HOME/.local
jupyter notebook --port $PORT &
IPYPROC=$!
echo ""
echo "Running jupyter notebook on port $PORT"
echo ""
echo ""
echo ""
echo "Sleeping for 4 hours"
sleep 4h
echo "Killing proc"
kill $SSHPROC
kill $IPYPROC
EOF


sbatch $subname
}

activate_above ()
{
    CUR_DIR=$(pwd)
    while [ ! $(pwd) == "/" ]
    do
        if [[  -e ./activate.sh  ]]
        then
            active_script="$(pwd)/activate.sh"
            echo "Sourcing $active_script"
            . $active_script
        fi

        cd ..
    done

    cd $CUR_DIR
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
function c ()
{
    cd $(ls -1 | fzf)
}

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

function sst ()
{
    job=$(qstat -u ndb245 | tail -n 1 | awk -F' ' '{print $1}')
    showstart  $job
}

# Print first matching group
function regexp() {
    perl -n -e "/$1/ && print \$1 . \"\n\""
}

. ~/.dotfiles/shell/aliases.sh
