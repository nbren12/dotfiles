source ~/.dotfiles/shell/aliases.sh

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

# find anaconda
set _PATHS /Users/noah/anaconda3/bin /Users/noah/anaconda/bin /Users/noah/bin

for pth  in $_CONDA_PATHS 
    set -gx PATH $_CONDA_PATHS $PATH
end


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

#gcloud
set -gx PATH $HOME/.google-cloud-sdk/bin $PATH

# Load fishmarks (http://github.com/techwizrd/fishmarks)
. $HOME/.fishmarks/marks.fish
