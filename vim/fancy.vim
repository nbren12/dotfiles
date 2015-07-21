" vim: ft=vim fdm=marker fo=tcr tw=79
"
" 07 Nov 2014
" -----------
"
" This is a new vimrc built from scratch based on the neobundle plugin
" manager. Some tips I have noticed:
"   1. Don't bother with python3 compilation. Just use jedi-vim with python2.7
"   and set the PYTHONPATH environmental variable to include the things you
"   want.
"
"   2. Use neosnippet, which is faster than ultisnips.
" 
" Nov 27, 2014
" ------------
"
"  Trying to install a good notetaking software, like notational velocity.
"  `nvim` seems promising, but requires some difficult to install depedencies.
"  For org-mode, just use emacs. Vim-notes is interesting but is not easily
"  exportable to pdf.
"
" Jul 21, 2015
" ------------
"
"  Back in vim land after using emacs for a while.  I am using neovim and Plug
"  for managing the plugins. Again, YouCompleteMe has proved to be a giant pain
"  in the ass to install.
call plug#begin('~/.nvim/plugged')

""" Essential Plugins
Plug 'kien/ctrlp.vim'     " file finding
Plug 'tComment'           " Commenting
Plug 'tpope/vim-fugitive' " Git
Plug 'tpope/vim-repeat'   " Repeat everything with  .
Plug 'godlygeek/tabular'  " Like regex-align
Plug 'justinmk/vim-sneak' " quick movement
Plug 'surround.vim'       " Parenthesis
Plug 'guns/vim-sexp'      " Lisp

"" Tmux
" Navigate between panes (very useful)
Plug 'christoomey/vim-tmux-navigator' " See https://github.com/christoomey/vim-tmux-navigator

" Send text to external REPL
Plug 'benmills/vimux'
 function! VimuxSlime()
  call VimuxSendText(@v)
  call VimuxSendKeys("Enter")
 endfunction


""" Nonessential
Plug 'bfredl/nvim-ipy'

""" Testing
Plug 'tpope/vim-fireplace'

" Themes
Plug 'sickill/vim-monokai'

" Required:
call plug#end()

" Required:
filetype plugin indent on
syntax on

" Run basic settings
ru vanilla.vim

" Ctrlp Settings
nmap <leader>fr :CtrlPMRUFiles<CR>

" Bidnings

 map <Leader>vp :VimuxPromptCommand<CR>
 map <Leader>vs "vy :call VimuxSlime()<CR>

" Themeing
colo monokai


