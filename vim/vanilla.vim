" Basic settings"{{{
set nocompatible
set mouse=a
set ignorecase		"search ignores case
set backspace=2     "Make backspace behave normally
set laststatus=2    " Always show status line
set listchars=""
set wildignore+=*.so,*.swp,*.zip
set wildignore+=*.Trash/*
set autochdir
set completeopt=menuone,longest,preview
set hidden          " Allows leaving ufinished buffers
set cursorline
set incsearch                   " Find as you type search
set hlsearch   " Highlight search
set tags=tags;/
set number
set tw=79

set suffixes+=.aux,.fls,.log   " Files to not search for

" set smartindent
set nowrap                      " Wrap long lines
set autoindent                  " Indent at the same level of the previous line
set shiftwidth=4                " Use indents of 4 spaces
set expandtab                   " Tabs are spaces, not tabs
set tabstop=4                   " An indentation every four columns
set softtabstop=4               " Let backspace delete indent
"set matchpairs+=<:>             " Match, to be used with %
set pastetoggle=<F12>           " pastetoggle (sane indentation on pastes)

let mapleader=" "
let maplocalleader = "-"
"}}}

"{{{ Basic Key maps

imap jk <Esc>


"Insert mode movement
imap <C-a> <C-O>0
imap <C-e> <C-O>$

" Spell checking
nmap <Leader>z 1z=

" Windows Management Shortcuts
nnoremap <C-h> <C-w>h
nnoremap <C-k> <C-w>k
nnoremap <C-j> <C-w>j
nnoremap <C-l> <C-w>l
nnoremap <C-n> <C-w>n
nnoremap <C-x> <C-w>x

nnoremap <leader>m :silent !open -a Marked.app '%:p'<cr>


" Misc bindings
nnoremap <Leader>d :bd <CR>
nnoremap <Leader>s :w <CR>
nnoremap tt ciwTrue
nnoremap tf ciwFalse
nnoremap  :noh <CR>

" Movement
" mapping to make movements operate on 1 screen line in wrap mode
function! ScreenMovement(movement)
 if &wrap
     return "g" . a:movement
 else
     return a:movement
 endif
endfunction
nnoremap <silent> <expr> j ScreenMovement("j")
nnoremap <silent> <expr> k ScreenMovement("k")
nnoremap <silent> <expr> 0 ScreenMovement("0")
nnoremap <silent> <expr> ^ ScreenMovement("^")
" nnoremap <silent> <expr> $ ScreenMovement("$")

nnoremap <Leader>t :TagbarToggle<CR>

map <Leader>5 <F5>


" quickfix space mapping
au FileType qf nnoremap <buffer> <Space> <CR><C-W>p
nnoremap <Leader>lc :copen <CR>
nnoremap <Leader>cl :close <CR>

" Folding
nnoremap <BS> za 
vnoremap <BS> zf
set foldnestmax=2


" Abbreviations

iab ndb Noah D. Brenowitz
iab nb Noah Brenowitz
iab em noah@cims.nyu.edu
iab teh the
iab adn and
iab fo of

" Date Time
iab <expr> dts strftime("%b %d, %Y")

"}}}

" Don't screw up folds when inserting text that might affect them, until
" leaving insert mode. Foldmethod is local to the window. Protect against
" screwing up folding when switching between windows.
autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif


au FileType python set fdm=indent
nmap <Leader>be ofrom IPython import embed; embed()
nmap <Leader>bb oimport ipdb; ipdb.set_trace()
nmap <Leader>bd silent bufdo %g/^\s*import ipdb; ipdb.set_trace()/d

au FileType fortran set sw=2 tabstop=2

syntax on


