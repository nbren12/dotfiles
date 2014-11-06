"vim: set sw=4 ts=4 sts=4 et tw=78 foldmethod=marker spell:
"
" Essential plugins
"
" - Fugitive
" - CtrlP
" - surround.vim
" - tComment
" - ultisnips
" - Tabularize
"
" Plugins to Avoid:
" - YouCompletMe (total pain in ass to install)
" - VimComplteMe (not compatible with ultisnips and not very flexible)
" - Powerline ( hard to install)
"
" Change Log:
"
" Aug 26, 2014:
"
" Attempted to install YouCompleteMe for the millionth time. Didn't work, and
" I received a "ycmd server crash" error. I am done with this plugin, never
" use again.
"
" Aug 28, 2014
"
" Mucked around with the automcpletion. Add some fugitive autocmds and maps
" from the vimcasts video. Using the airline plugin.
"
" Aug 29, 2014 
"
" Installed tim pope's repeat.vim plugin for repeating commands in other
" plugins, like surround. Also installed vim easy motion.
"
" Oct 13, 2014 
"
" Installed latex-box again. It only gets really slow when using the
" autopairs plugin. Solution: disable all pairs matching in latex files:
"      au Filetype tex let b:AutoPairs = {}
" Switched folding to f key from tab. <C-I> did not work as expected with Tab
" mapping.


set nocompatible	"has to be first line
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
set lines=40
set columns=80

" Basic Keystrokes
" let mapleader=','
imap jj <Esc>
"Get rid of highlighting after search
" nnoremap <CR> :noh<CR><CR>
filetype off                   " required!

" Sync the clipboard with the registers
" set clipboard=unnamed

" Plugins {{{


set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'

Bundle 'surround.vim'
Bundle 'JuliaLang/julia-vim'
Bundle 'MatlabFilesEdition' 
Bundle 'fortran.vim'
Bundle 'Lokaltog/vim-easymotion'
nmap <Space> <Plug>(easymotion-prefix)
vmap <Space> <Plug>(easymotion-prefix)

" Tim Pop  plugins
" Many useful shortucts
Bundle 'tpope/vim-unimpaired'

" Use repeat to use '.' with surround.
Bundle 'tpope/vim-repeat.git'

Bundle 'https://github.com/tpope/vim-fugitive.git'
autocmd BufReadPost fugitive://* set bufhidden=delete
autocmd User fugitive
  \ if fugitive#buffer().type() =~# '^\%(tree\|blob\)$' |
  \   nnoremap <buffer> .. :edit %:h<CR> |
  \ endif

Bundle 'bling/vim-airline'

Bundle 'mileszs/ack.vim'
Bundle 'majutsushi/tagbar'
let g:tagbar_type_tex = {
    \ 'ctagstype' : 'latex',
    \ 'kinds'     : [
        \ 's:sections',
        \ 'g:graphics:0:0',
        \ 'l:labels',
        \ 'r:refs:1:0',
        \ 'p:pagerefs:1:0'
    \ ],
    \ 'sort'    : 0,
\ }

" Bundle 'taglist.vim'
Bundle 'a.vim'
Bundle 'tComment'
" Bundle 'scrooloose/nerdtree'
" map <C-e> :NERDTreeToggle<CR>
" let NERDTreeShowBookmarks=1

Bundle 'godlygeek/tabular'

nmap <Leader>a& :Tabularize /& <CR>
nmap <Leader>a= :Tabularize /= <CR>
vmap <Leader>a& :Tabularize /& <CR>
vmap <Leader>a= :Tabularize /= <CR>

Bundle 'Shuogo/neomru.vim'
Bundle 'Shuogo/vimproc.vim'
Bundle 'Shuogo/unite.vim'


nnoremap <C-p> :Unite  -buffer-name=files buffer file_rec/git file_mru file file_rec<CR>
call unite#custom#profile('default', 'context', {
\   'start_insert': 0,
\   'winheight': 10,
\   'direction': 'botright',
\ })


nnoremap <Space>/ :Unite grep:.<CR>
nnoremap <Leader>q :Unite -quick-match buffer<CR>

"Code completion"{{{
au FileType * exec("setlocal dictionary+=".$HOME."/.vim/dictionaries/".expand('<amatch>'))
set complete+=k



" Bundle 'neocomplcache'
" 2014-06-15 01:29: Using neocomplete, turned off supertab, and many
" others

Bundle 'ervandew/supertab'
let g:SuperTabDefaultCompletionType = "context"

Bundle 'davidhalter/jedi-vim'
autocmd FileType python setlocal omnifunc=jedi#completions
let g:jedi#usages_command = "<leader>z"
let g:jedi#popup_on_dot = 0
let g:jedi#rename_command = "<leader>rr"
map <Leader>b Oimport ipdb; ipdb.set_trace() # BREAKPOINT<C-c>

" C++ Stuff
Bundle 'Rip-Rip/clang_complete'
let g:clang_library_path  = "/Library/Developer/CommandLineTools/usr/lib"
Bundle 'stlrefvim'


if 0


    Bundle 'Shougo/neocomplete.vim'
    let g:neocomplete#enable_at_startup = 1
    let g:neocomplete#disable_auto_complete = 0


    let g:neocomplete#force_overwrite_completefunc = 1

    if !exists('g:neocomplete#force_omni_input_patterns')
    let g:neocomplete#force_omni_input_patterns = {}
    endif
    let g:neocomplete#force_omni_input_patterns.python =
    \ '[^. *\t]\.\w*\|\h\w*::'

    " For Clang complete from neocomplete docs
    let g:neocomplete#force_omni_input_patterns.c =
          \ '[^.[:digit:] *\t]\%(\.\|->\)\w*'
    let g:neocomplete#force_omni_input_patterns.cpp =
          \ '[^.[:digit:] *\t]\%(\.\|->\)\w*\|\h\w*::\w*'
    let g:neocomplete#force_omni_input_patterns.objc =
          \ '\[\h\w*\s\h\?\|\h\w*\%(\.\|->\)'
    let g:neocomplete#force_omni_input_patterns.objcpp =
          \ '\[\h\w*\s\h\?\|\h\w*\%(\.\|->\)\|\h\w*::\w*'

    let g:SuperTabDefaultCompletionType = "<c-x><c-u>"

    let g:clang_complete_auto = 0
    let g:clang_auto_select = 0
endif

Bundle 'scrooloose/syntastic'
" let g:syntastic_mode_map = { "mode": "active",
"                            \ "passive_filetypes": ["python"] }
let g:syntastic_mode_map = { "mode": "active"}
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = ' -std=c++11 -stdlib=libc++'
let g:syntastic_python_python_exec = '~/anaconda3/bin/python'
let g:syntastic_python_checkers = ['flake8']
" let g:syntastic_quiet_messages = { "level": "warnings" }

nnoremap <F7> :SyntasticCheck <CR>

Bundle 'FSwitch'


"" Snippets:
" Sat Aug 10 01:52:15 EDT 2013: UltiSnips is too slow
" 2014-06-15 00:53   Added ultisnips again. it's awesome.
Bundle 'honza/vim-snippets'
Bundle 'SirVer/ultisnips'
let g:UltiSnipsListSnippets="<c-tab>"
let g:UltiSnipsSnippetDirectories=["UltiSnips", "mysnippets"]
"}}}


" Autoclosing
" Tried many plugins, Townk is bad, AutoClose is bad
" http://stackoverflow.com/questions/883437/how-do-i-get-vim-to-automatically-put-ending-braces/883522#883522

Bundle "jiangmiao/auto-pairs"
" au Filetype tex let b:AutoPairs = {"{": "}", "$": "$"}
au Filetype tex let b:AutoPairs = {}

" Colorschemes"{{{
Bundle 'Wombat'
Bundle 'Lokaltog/vim-distinguished'
Bundle 'jellybeans.vim'
Bundle 'zeis/vim-kolor'
Bundle 'altercation/vim-colors-solarized'
Bundle 'pyte'
"}}}


" Latex
" Need to download the latest version from sourcefourge and install psutil
Bundle 'LaTeX-Box-Team/LaTeX-Box'
let g:LatexBox_latexmk_async = 1
let g:tex_flavor = "latex"
let g:LatexBox_Folding = 1
let g:LatexBox_fold_automatic = 0 " Needed for fast ultisnips expansion
set suffixes+=.log,.aux,.bbl,.fdb_latexmk,.latexmain,.fls,.idx,.gz




" For matching in fortran"
Bundle 'matchit.zip'

Bundle 'ivanov/vim-ipython'


" }}}

" User Interface {{{
    set hlsearch
    set viewoptions=folds,options,cursor,unix,slash " Better Unix / Windows compatibility
    " syntax on

    if has("gui_running")
        " colorscheme wombat
        " colorscheme zellner
        " colorscheme jellybeans
        set background=dark
        " colorscheme solarized
        colorscheme pyte

        "set guifont=Mono\ Regular:h14,Menlo\ Regular:h14,Consolas\ Regular:h14,Courier\ New\ Regular:h16
        set guifont=Monospace\ 10,Monaco,Consolas:h12
        " turn off the toolbar
        set guioptions-=T
    else
        " colorscheme desert
    endif

    "Folding "
    set foldmethod=marker
    set wrap " word wrap
    autocmd FileType c,matlab,cpp,fortran setlocal foldmethod=syntax

    " let g:fortran_free_source=1
    autocmd FileType python setlocal foldmethod=indent

    " This is why autocomplete is abysmally slow sometimes
    " Don't screw up folds when inserting text that might affect them, until
    " leaving insert mode. Foldmethod is local to the window. Protect against
    " screwing up folding when switching between windows.
    autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
    autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif

    " Accept Mouse Input

    set number
    let g:fullscreen_colorscheme = "iawriter"
    let g:fullscreen_font = "DejaVu\ Sans\ Mono\ Book\ 12"
    let g:normal_colorscheme = "default"
    let g:normal_font=&guifont

" }}}

" Tab behavior {{{

    " Strip whitespace {
    function! StripTrailingWhitespace()
        " To disable the stripping of whitespace, add the following to your
        " .vimrc.local file:
        "   let g:spf13_keep_trailing_whitespace = 1
        if !exists('g:spf13_keep_trailing_whitespace')
            " Preparation: save last search, and cursor position.
            let _s=@/
            let l = line(".")
            let c = col(".")
            " do the business:
            %s/\s\+$//e
            " clean up: restore previous search history, and cursor position
            let @/=_s
            call cursor(l, c)
        endif
    endfunction
    autocmd FileType c,cpp,java,php,javascript,python,twig,xml,yml autocmd BufWritePre <buffer> call StripTrailingWhitespace()
    autocmd BufNewFile,BufRead *.html.twig set filetype=html.twig

    " set smartindent
    set nowrap                      " Wrap long lines
    set autoindent                  " Indent at the same level of the previous line
    set shiftwidth=4                " Use indents of 4 spaces
    set expandtab                   " Tabs are spaces, not tabs
    set tabstop=4                   " An indentation every four columns
    set softtabstop=4               " Let backspace delete indent
    "set matchpairs+=<:>             " Match, to be used with %
    set pastetoggle=<F12>           " pastetoggle (sane indentation on pastes)
    "set comments=sl:/*,mb:*,elx:*/  " auto format comment blocks
    " Remove trailing whitespaces and ^M chars


" }}}

" Abbreviations "{{{
iab ndb Noah D. Brenowitz
iab nb Noah Brenowitz
iab em noah@cims.nyu.edu
iab teh the
iab adn and
iab fo of

"}}}

" Shorcuts {{{

    " toggle fullscreen mode
    " nmap  <C-F> :call ToggleDistractionFreeWriting()<CR>

    " Make. cant use C-m, because that means return
    nnoremap mm :make<CR>

    nnoremap <leader>m :silent !open -a Marked.app '%:p'<cr>

    " Search back
    nnoremap <leader>f F


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

    " Misc bindings
    nnoremap <Leader>d :bd <CR>
    nnoremap <Leader>s :w <CR>
    nnoremap tt ciwTrue
    nnoremap tf ciwFalse
    nnoremap  :noh <CR>


    "Insert mode movement
    imap <C-a> <C-O>0
    imap <C-e> <C-O>$

    " Spell checking
    nmap <Leader>z 1z=

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

     " Ctags Code browsing shortcuts
     map <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
     map <A-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>
     map <leader>bta :!/usr/local/bin/ctags -R .<CR>
     set tags=tags;/

    nnoremap <Leader>t :TagbarToggle<CR>

    map <Leader>5 <F5>

    " Fugitive"
    nnoremap <silent> <Leader>gs  :Gstatus<CR>

    " quickfix space mapping
    au FileType qf nnoremap <buffer> <Space> <CR><C-W>p
    nnoremap <Leader>lc :copen <CR>
    nnoremap <Leader>cl :close <CR>

    " Folding
    nnoremap <BS> za 
    vnoremap <BS> zf
    set foldnestmax=2




"}}}
" Abreviations "{{{

    " Date Time
    iab <expr> dts strftime("%b %d, %Y")
    iab teh the
    iab adn and
    iab fo of


"}}}


" Extras"{{{

" Google search "{{{
if has('mac')
    nmap <Leader>gg :silent !open -a Firefox.app "http://www.google.com/?q=<cword>" <CR>
endif"}}}

" MVIM to iTerm "{{{
function! SendToTerminal(args)
  execute ":silent !mvim_to_iterm '" . a:args . "'"

endfunction

function! ClearTerminal()
  call SendToTerminal("clear")
endfunction

function! SyncDirs()
    call SendToTerminal("cd %:p:h")
endfunction


let g:sync_mvim_to_iterm_dir = 0
function! ToggleSyncDirs()
    if g:sync_mvim_to_iterm_dir
        let g:sync_mvim_to_iterm_dir = 0
        echom "Directories not Synced!"
        autocmd! sync_dir_files
    else
        augroup sync_dir_files
            autocmd!
            autocmd BufEnter *.txt,*.py,*.c,*.cpp,*.f90 :call SyncDirs()
        augroup END
        echom "Directories Synced!"
        let g:sync_mvim_to_iterm_dir = 1
    endif
endfunction

nnoremap <Leader>sdd :call ToggleyncDirs()<CR>
nnoremap <Leader>sd  :call SyncDirs()<CR>
"}}}
"}}}

filetype plugin indent on
syntax on
