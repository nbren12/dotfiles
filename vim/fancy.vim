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
"  for managing the plugins
call plug#begin('~/.nvim/plugged')

Plug 'kien/ctrlp.vim'
Plug 'surround.vim'
Plug 'tComment'
Plug 'tpope/vim-fugitive'
Plug 'tpope/vim-repeat'
Plug 'majutsushi/tagbar'
Plug 'godlygeek/tabular'
Plug 'justinmk/vim-sneak'
Plug 'terryma/vim-expand-region'
Plug 'guns/vim-sexp'
Plug 'bfredl/nvim-ipy'


" Required:
call plug#end()

" Required:
filetype plugin indent on
syntax on

" Run basic settings
ru vanilla.vim

" Ctrlp Settings
nmap <leader>fr :CtrlPMRUFiles<CR>


