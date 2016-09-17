#    source the local profile first

############################################################
#                  Colorscheme and Prettiness
############################################################

# This sets LS Colors differently for darwin and linux
if [ "$TERM" != "dumb" ]; then
 if [ $(uname) == "Linux"  ]; then
   # Linux
   alias ls='ls --color=auto'
   LS_COLORS='di=33:fi=0:ln=95:pi=5:so=5:bd=5:cd=5:or=37:mi=0:ex=31:*.rpm=90'
 else
   # OS X   
   alias ls='ls -G'
   export LSCOLORS=dxfxcxdxbxegedabagacad
 fi
 #This is for everyone       
 export CLICOLOR=1
fi

# export PS1='\[\033[01;32m\]\u@\h\[\033[01;34m\] \W \$\[\033[00m\] '
# export PS1=':; '
# export PS1='[\[\033[01;32m\]\u\[\033[01;34m\] \W \[\033[00m\]]'
export PS1='[\[\033[01;34m\]\W\[\033[0m\]] '

# vim mode
set -o vi
export EDITOR='/usr/bin/env vim'
#export EDITOR='emacsclient -c --alternate-editor="vim"'
export GIT_EDITOR=$EDITOR
export PAGER=less
export LANG='en_US.UTF-8'


if [[ -e $HOME/.git-completion.bash ]]
then
    source $HOME/.git-completion.bash
fi

# z
[[ -e ~/usr/bin/z.sh ]] && . ~/usr/bin/z.sh

# fzf
[ -f ~/.fzf.bash ] && . ~/.fzf.bash
