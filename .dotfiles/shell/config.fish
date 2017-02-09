. ~/.dotfiles/shell/aliases.sh
. ~/anaconda3/etc/fish/conf.d/conda.fish

set -gx PATH /Users/noah/bin /Users/noah/anaconda3/bin /usr/local/texlive/2014/bin/x86_64-darwin /Applications/Julia-0.5.app/Contents/Resources/julia/bin /Applications/Emacs.app//Contents/MacOS/bin $PATH 

# Load fishmarks (http://github.com/techwizrd/fishmarks)
. $HOME/.fishmarks/marks.fish

# julia setup
set -gx PYTHON /Users/noah/anaconda3/bin/python
set -gx JUPYTER /Users/noah/anaconda3/bin/jupyter
