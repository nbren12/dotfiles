source ~/.dotfiles/shell/aliases.sh
set -gx EDITOR 'emacsclient -c'

# install fisher if necessary
if [ ! -e ~/.config/fish/functions/fisher.fish ]
  curl -Lo ~/.config/fish/functions/fisher.fish --create-dirs https://git.io/fisher
end

# fzf
set -gx PATH $HOME/.fzf/bin $PATH

if not type -q fzf 
  echo "Download FZF..."
  git clone --depth 1 https://github.com/junegunn/fzf.git ~/.fzf
  ~/.fzf/install --bin
end

# find anaconda
set _PATHS /Users/noah/anaconda3/bin /Users/noah/anaconda/bin /Users/noah/bin \
   $HOME/Dropbox/gnl/bin

for pth in $_PATHS 
    [ -d $pth ]; and set -gx PATH $pth $PATH
end




# date
alias today='date +%F'
alias td='today'

# emacs aliases
alias emd='/Applications/Emacs.app/Contents/MacOS/Emacs --daemon'
alias em='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient --no-wait'
alias emn='/Applications/Emacs.app/Contents/MacOS/bin/emacsclient -c
--no-wait'
alias t='task'

# NYU HPC setup
function hpc
    echo "**********************************"
    echo "Connecting to NYU HPC Bastion Node"
    echo "**********************************"
    ssh -N hpc
end

#gcloud
# set -gx PATH $HOME/.google-cloud-sdk/bin $PATH

# Load fishmarks (http://github.com/techwizrd/fishmarks)
. $HOME/.fishmarks/marks.fish

# load system specific configs
[ -f $HOME/.env ]; and source $HOME/.env

# key bindings
set -g fish_key_bindings fish_default_key_bindings
# set -g fish_key_bindings fish_vi_key_bindings

# ncdump helper
function ncd
    ncdump -h $argv | less
end

# abbrevs
abbr -a gcm 'git commit --amend' 
abbr -a gco 'git checkout'
abbr -a gcn 'git clean -nd '
abbr -a c 'conda'

# add autojump if it's present
[ -f /usr/local/share/autojump/autojump.fish ]; and source /usr/local/share/autojump/autojump.fish

