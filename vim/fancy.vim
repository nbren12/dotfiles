" vim: ft=vim fdm=marker fo=tcr tw=79
"
" Jan 20, 2016
" ------------
"
"  - Disable jedi-vim because it is slow and annoying
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
" 
" Feb 25, 2016
" ------------
"
"  Installing FZF key bindings, and putting the sourcing of vanilla.vim in the
"  front of the file. Also installed ultisnips with lazy loading.


" Run basic settings before anything else
ru  vanilla.vim

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                   BEGIN PLUG SECTION
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

call plug#begin('~/.nvim/plugged')

""" Essential Plugins
Plug 'tComment'           " Commenting
Plug 'tpope/vim-fugitive' " Git
Plug 'tpope/vim-repeat'   " Repeat everything with  .
Plug 'godlygeek/tabular'  " Like regex-align
Plug 'justinmk/vim-sneak' " quick movement
Plug 'surround.vim'       " Parenthesis
" Plug 'guns/vim-sexp'      " Lisp
" Plug 'tpope/vim-sexp-mappings-for-regular-people' " Better bindings
Plug 'airblade/vim-gitgutter' " see git diffs near the line no
Plug 'airblade/vim-rooter'

" Fuzzy file finding
Plug 'kien/ctrlp.vim'     " file finding

""" TMUX
Plug 'christoomey/vim-tmux-navigator' " See https://github.com/christoomey/vim-tmux-navigator

" Send text to external REPL
Plug 'benmills/vimux'
 function! VimuxSlime()
  call VimuxSendText(@v)
  call VimuxSendKeys("Enter")
 endfunction

""" Language specific plugins 
" Julia
" Plug 'JuliaLang/julia-vim'

" Python
" let g:jedi#force_py_version = 3
" Plug 'davidhalter/jedi-vim'
" let g:jedi#popup_on_dot = 0

" Clojure 
Plug 'tpope/vim-fireplace'

""" Testing
Plug 'baruchel/vim-notebook' 
Plug 'tpope/vim-markdown'

" Snippets: use lazy loading
Plug 'SirVer/ultisnips',  {'on': []}  | Plug 'honza/vim-snippets'
autocmd InsertEnter * call plug#load('ultisnips')

" FZF
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim' | Plug 'dbakker/vim-projectroot'

command! FZFMru call fzf#run({
            \  'source':  v:oldfiles,
            \  'sink':    'e',
            \  'options': '-m -x +s',
            \  'down':    '40%'})

nnoremap <leader>ff :FZF<CR>
nnoremap <leader>fs :Snippets<CR>
nnoremap <leader>fr :FZFMru<CR>
imap <c-x><c-l> <plug>(fzf-complete-line)


Plug 'ag.vim'
" Plug 'Shougo/deoplete.nvim'
" let g:deoplete#enable_at_startup = 1

Plug 'vim-pandoc/vim-pandoc'
Plug 'vim-pandoc/vim-pandoc-syntax'
let g:pandoc#biblio#bibs= ["~/Dropbox/Papers/zotero.bib",]
let g:pandoc#biblio#use_bibtool=1


Plug 'terryma/vim-multiple-cursors'
"
" VIM/NVIM specific plugins
"
if has('nvim')
Plug 'bfredl/nvim-ipy'
Plug 'benekastah/neomake'
endif

" Required:
call plug#end()
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"                   END OF PLUG SECTION
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Required:
filetype plugin indent on
syntax on


" Bidnings

 map <Leader>vp :VimuxPromptCommand<CR>
 map <Leader>vs "vy :call VimuxSlime()<CR>

"
" VIM/NVIM Settings
"

if has('nvim')
" vim-ipy 
autocmd FileType python nmap <F5> ggVG<Plug>(IPy-Run)<C-o><C-o>

:tnoremap <Esc> <C-\><C-n>
:tnoremap <A-h> <C-\><C-n><C-w>h
:tnoremap <A-j> <C-\><C-n><C-w>j
:tnoremap <A-k> <C-\><C-n><C-w>k
:tnoremap <A-l> <C-\><C-n><C-w>l
:nnoremap <A-h> <C-w>h
:nnoremap <A-j> <C-w>j
:nnoremap <A-k> <C-w>k
:nnoremap <A-l> <C-w>l


else
endif

