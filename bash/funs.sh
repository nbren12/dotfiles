
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

# shortcut to fire up emacs 
function em {
 command emacs "$@" &
}

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
