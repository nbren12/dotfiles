# .bashrc

# Source global definitions
if [ -f /etc/bashrc ]; then
	. /etc/bashrc
fi

# Load user aliases and funs
source ~/.dotfiles/shell/bash/common.sh
source ~/.dotfiles/shell/bash/funalias.sh

