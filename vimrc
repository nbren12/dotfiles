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
"NeoBundle Scripts-----------------------------"{{{
if has('vim_starting')
  set nocompatible               " Be iMproved

  " Required:
  set runtimepath+=~/.vim/bundle/neobundle.vim/
endif

" Required:
call neobundle#begin(expand('~/.vim/bundle'))

" Let NeoBundle manage NeoBundle
" Required:
NeoBundleFetch 'Shougo/neobundle.vim'

" Add or remove your Bundles here:

" Essential Plugins

NeoBundle 'surround.vim'
NeoBundle 'tComment'
NeoBundle 'tpope/vim-fugitive'
NeoBundle 'Lokaltog/vim-easymotion'

" Testing plugins
NeoBundle 'scrooloose/syntastic'
NeoBundle 'davidhalter/jedi-vim'
NeoBundle 'ivanov/vim-ipython'
NeoBundle 'Shougo/neocomplete.vim'
NeoBundle 'Shougo/neosnippet.vim'
NeoBundle 'Shougo/neosnippet-snippets'
NeoBundle 'honza/vim-snippets'
NeoBundle 'flazz/vim-colorschemes'
NeoBundle 'Shougo/neomru.vim'
NeoBundle 'Shougo/vimproc.vim', {
\ 'build' : {
\     'windows' : 'tools\\update-dll-mingw',
\     'cygwin' : 'make -f make_cygwin.mak',
\     'mac' : 'make -f make_mac.mak',
\     'linux' : 'make',
\     'unix' : 'gmake',
\    },
\ }

NeoBundle 'Shougo/unite.vim'

" Required:
call neobundle#end()

" Required:
filetype plugin indent on
syntax on

" If there are uninstalled bundles found on startup,
" this will conveniently prompt you to install them.
NeoBundleCheck
"End NeoBundle Scripts-------------------------"}}}

" Basic settings"{{{

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
set columns=80
set tags=tags;/
set number

" set smartindent
set nowrap                      " Wrap long lines
set autoindent                  " Indent at the same level of the previous line
set shiftwidth=4                " Use indents of 4 spaces
set expandtab                   " Tabs are spaces, not tabs
set tabstop=4                   " An indentation every four columns
set softtabstop=4               " Let backspace delete indent
"set matchpairs+=<:>             " Match, to be used with %
set pastetoggle=<F12>           " pastetoggle (sane indentation on pastes)

let maplocalleader = "-"
"}}}

"{{{ Basic Key maps

imap jj <Esc>


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
nnoremap <Leader>v <C-w>v
nnoremap <Leader>wh <C-w>H
nnoremap <Leader>wj <C-w>J
nnoremap <Leader>c <C-w>c
nnoremap <Leader>o <C-w>o

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




"}}}

" Don't screw up folds when inserting text that might affect them, until
" leaving insert mode. Foldmethod is local to the window. Protect against
" screwing up folding when switching between windows.
autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif

colo wombat

" Plugin Settings"{{{

" Fugitive
nnoremap <silent> <Leader>gs  :Gstatus<CR>

nnoremap <C-p> :Unite  -buffer-name=files buffer file_rec/git file_mru file file_rec<CR>

call unite#custom#profile('default', 'context', {
\   'start_insert': 0,
\   'winheight': 10,
\   'direction': 'botright',
\ })


nnoremap <Space>/ :Unite grep:.<CR>
nnoremap <Leader>q :Unite -quick-match buffer<CR>
nnoremap <Leader>r :Unite  file_mru<CR>



" Tab neosnippet

let g:UltiSnipsSnippetDirectories=["UltiSnips", "mysnippets"]


" Auto completion
let g:neocomplete#enable_at_startup = 1"


" Latex Box
let g:LatexBox_Folding = 1

nmap <Space> <Plug>(easymotion-prefix)
vmap <Space> <Plug>(easymotion-prefix)


" Snippet Settings

" Plugin key-mappings.
imap <C-k>     <Plug>(neosnippet_expand_or_jump)
smap <C-k>     <Plug>(neosnippet_expand_or_jump)
xmap <C-k>     <Plug>(neosnippet_expand_target)

" SuperTab like snippets' behavior.
imap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: pumvisible() ? "\<C-n>" : "\<TAB>"
smap <expr><TAB> neosnippet#expandable_or_jumpable() ?
\ "\<Plug>(neosnippet_expand_or_jump)"
\: "\<TAB>"

" For snippet_complete marker.
" if has('conceal')
"   set conceallevel=2 concealcursor=i
" endif

" Enable snipMate compatibility feature.
let g:neosnippet#enable_snipmate_compatibility = 1
let g:neosnippet#snippets_directory = "~/.vim/mysnippets"
let g:syntastic_python_python_exec = '~/anaconda3/bin/python3'

" }}}
