set nocompatible        " Has to be the first line

set autoindent          " auto indenting
set backspace=2         " backspace insert mode works like normal editor
set number              " line numbers
set numberwidth=4

set nobackup            " get rid of anoying ~file

" Search incrementally with highlight
set is hls


" Tabs
set tabstop=8 softtabstop=0 expandtab shiftwidth=4 smarttab

" Appearence

set bg=dark
set ruler

let mapleader="\\"

" Keybindings
inoremap jj <Esc> 

" Window management keybindings
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l
nnoremap <C-h> <C-w>h

" Change to current directory of current file
nnoremap <Leader>cd <silent> :cd %:p:h<CR>

syntax on               " syntax highlighting
filetype indent on      " activates indenting for files

