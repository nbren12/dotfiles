. ~/.dotfiles/shell/aliases.sh
. ~/anaconda3/etc/fish/conf.d/conda.fish

set -gx PATH /Users/noah/bin /Users/noah/anaconda3/bin /usr/local/texlive/2016/bin/x86_64-darwin /Applications/Julia-0.5.app/Contents/Resources/julia/bin $PATH 

# Load fishmarks (http://github.com/techwizrd/fishmarks)
. $HOME/.fishmarks/marks.fish

# julia setup
set -gx PYTHON /Users/noah/anaconda3/bin/python
set -gx JUPYTER /Users/noah/anaconda3/bin/jupyter

alias emd='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
alias em='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --no-wait'
alias emn='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c
--no-wait'

