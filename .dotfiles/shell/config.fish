. ~/.dotfiles/shell/aliases.sh
. ~/anaconda/etc/fish/conf.d/conda.fish

# install fisher if necessary
if [ ! -e ~/.config/fish/functions/fisher.fish ]
  curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs https://git.io/fisher
end

# fzf
set -gx PATH $HOME/.fzf/bin $PATH

if not type -q fzf -h
  echo "Download FZF..."
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install --bin
end


set -gx PATH /Users/noah/bin /Users/noah/anaconda/bin $PATH


# date
alias today='date +%F'

# emacs aliases
alias emd='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
alias em='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --no-wait'
alias emn='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c
--no-wait'

# NYU HPC setup
function hpc
    echo "**********************************"
    echo "Connecting to NYU HPC Bastion Node"
    echo "**********************************"
    ssh -N hpc
end

# Load fishmarks (http://github.com/techwizrd/fishmarks)
. $HOME/.fishmarks/marks.fish
