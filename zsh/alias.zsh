
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

if [[ $(uname) == "Darwin" ]] 
then
    alias g='mvim --servername VIM1 --remote-silent'
    alias mlab='/Applications/MATLAB_R2013a.app/bin/matlab -nodesktop -nosplash'
    alias vim='mvim -v'
    alias marked="open -a Marked.app"

else
    alias g='gvim --servername VIM1 --remote-silent'
    alias mlab='matlab -nodisplay'
    alias vim='vim -X'
fi
    
