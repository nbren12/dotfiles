# fix for spacemacs
function fish_title
  true
end

set -gx PATH /Library/Tex/texbin $PATH

## aliases
alias vim nvim

alias ed 'emacs --daemon'
alias ec 'emacsclient'
alias ecc 'emacsclient -c'

alias p 'pushd'
alias o 'popd'
alias d 'dirs'
alias lmk 'latexmk -pdf'