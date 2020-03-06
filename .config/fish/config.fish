source ~/.dotfiles/shell/aliases.sh

# find anaconda
set _PATHS /home/noahb/miniconda3/bin /Users/noah/anaconda3/bin /Users/noah/anaconda/bin /Users/noah/bin \
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

#gcloud
# set -gx PATH $HOME/.google-cloud-sdk/bin $PATH

# load system specific configs
[ -f $HOME/.env ]; and source $HOME/.env

# key bindings
fish_vi_key_bindings

# ncdump helper
function ncd
    ncdump -h $argv | less
end

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
eval /home/noahb/miniconda3/bin/conda "shell.fish" "hook" $argv | source
# <<< conda initialize <<<

eval (direnv hook fish)

set -gx EDITOR vim
set -gx PATH /snap/google-cloud-sdk/current/bin $PATH
