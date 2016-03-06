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
