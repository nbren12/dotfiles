"vim: set sw=4 ts=4 sts=4 et tw=78 foldmethod=marker spell:

set nocompatible	"has to be first line
set ignorecase		"search ignores case
set backspace=2     "Make backspace behave normally

" Basic Keystrokes
let mapleader=','
imap jj <Esc>

"Get rid of highlighting after search
" nnoremap <CR> :noh<CR><CR>
filetype off                   " required!

" Sync the clipboard with the registers
set clipboard=unnamed

" Proving Grounds for various plugins
" Bundle 'VOoM'

" Plugins {{{
    set rtp+=~/.vim/bundle/vundle/
    call vundle#rc()
    
    " Markdown plugin
    Bundle 'Markdown'
    "
    " NCL Syntax Highlighting
    Bundle 'https://github.com/xiexinyls/vim.git'
    au BufRead,BufNewFile *.ncl set filetype=ncl

    Bundle 'notes.vim'
    
    " Bundle 'vimwiki'
    " let g:vimwiki_list = [{'path': '~/Dropbox/notes',
    "                    \ 'syntax': 'markdown', 'ext': '.md'}]
    Bundle 'fugitive.vim'

    Bundle 'taglist.vim'

    " Bundle 'vim-ipython'

    " let Vundle manage Vundle
    " required! 
    Bundle 'gmarik/vundle'

    " ctrlp {
        Bundle 'ctrlp.vim'
        " let g:ctrlp_working_path_mode = 1
        nnoremap <silent> <D-t> :CtrlP<CR>
        nnoremap <silent> <D-r> :CtrlPMRU<CR>
        nnoremap <silent> <Leader>r :CtrlPMRU<CR>
        let g:ctrlp_custom_ignore = {
            \ 'dir':  '\.git$\|\.hg$\|\.svn$',
            \ 'file': '\.exe$\|\.so$\|\.dll$' }

        " let g:ctrlp_user_command = {
        "     \ 'types': {
        "         \ 1: ['.git', 'cd %s && git ls-files'],
        "         \ 2: ['.hg', 'hg --cwd %s locate -I .'],
        "     \ },
        "     \ 'fallback': 'find %s -type f'
        " \ }
    "}

    "Code completion
    Bundle 'neocomplcache'
    let g:neocomplcache_enable_at_startup = 1 

    Bundle 'snipMate'
    Bundle 'tComment'

    
    Bundle 'surround.vim'
    " NERDTree for file browsing {
        Bundle 'scrooloose/nerdtree'
        map <C-e> :NERDTreeToggle<CR>
        let NERDTreeShowBookmarks=1
    " }
    " Bundle "Townk/vim-autoclose"
    
    " Colorscheme
    Bundle 'Wombat'
    Bundle 'Lokaltog/vim-distinguished'
    Bundle 'jellybeans.vim'

    "SuperTab
    Bundle 'SuperTab'

    " Bundle 'VimOrganizer'
    " TODO : This messes up the <C-j> mapping
    "
    Bundle 'laktek/distraction-free-writing-vim'
    Bundle 'jcf/vim-latex'
    let g:Tex_ViewRule_pdf = 'Skim'
    let g:tex_flavor='latex'
    let g:Tex_TreatMacViewerAsUNIX = 1
    "Fix the C-j nonsense in non latex files"
    if &filetype != 'tex'
        imap <Leader>#$ <Plug>IMAP_JumpForward
        nmap <Leader>#$ <Plug>IMAP_JumpForward
        vmap <Leader>#$ <Plug>IMAP_JumpForward
        vmap <Leader>#$ <Plug>IMAP_DeleteAndJumpForward
    endif
    
" }}}

" User Interface {{{
    set hlsearch
    set viewoptions=folds,options,cursor,unix,slash " Better Unix / Windows compatibility
    syntax on

    if has("gui_running")
        colorscheme wombat
        "set guifont=Mono\ Regular:h14,Menlo\ Regular:h14,Consolas\ Regular:h14,Courier\ New\ Regular:h16
        set guifont=DejaVu\ Sans\ Mono\ Book\ 10,Consolas:h13
        " turn off the toolbar
        set guioptions-=T
    elseif &t_Co == 256
        colorscheme distinguished 
    endif


    set foldmethod=marker
    set wrap " word wrap

    " Accept Mouse Input
    set mouse=a

    set number
    let g:fullscreen_colorscheme = "iawriter"
    let g:fullscreen_font = "DejaVu\ Sans\ Mono\ Book\ 12"
    let g:normal_colorscheme = "wombat"
    let g:normal_font=&guifont
    set incsearch                   " Find as you type search
    
" }}}
" Formatting {{{

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
    autocmd FileType c,cpp,java,php,javascript,python,twig,xml,yml autocmd BufWritePre <buffer> call StripTrailingWhitespace()
    autocmd BufNewFile,BufRead *.html.twig set filetype=html.twig

" }}}
 
" Shorcuts {{{

    " toggle fullscreen mode
    nmap  <C-F> :call ToggleDistractionFreeWriting()<CR>

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

    "Insert mode movement
    imap <C-a> <C-O>0
    imap <C-e> <C-O>$

    nmap k gk
    nmap j gj
    " Ctags Code browsing shortcuts
    map <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
    map <A-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>

    nnoremap <Leader>t :TlistToggle<CR>
    nnoremap yg "+y
    vnoremap yg "+y
    
    map <Leader>5 <F5>
        
"}}}

" Programming {{{
    " CTags Browsing
    set tags=./tags,./../tags,./*/tags
    " nmap <S-F> :set syntax=fortran<CR>:let b:fortran_fixed_source=!b:fortran_fixed_source<CR>:set syntax=text<CR>:set syntax=fortran<CR>
    let fortran_do_enddo=1
    let fortran_more_precise=1
    let fortran_free_source=1
    let fortran_have_tabs=1

    " Fix Indenting Behavior with #
    inoremap # X#   

"}}}

set listchars=""

" set wildignore+=*/tmp/*,*.so,*.swp,*.zip
" set wildignore+=*.Trash/*,*/Library/*

set autochdir
let g:notes_directory = '~/Dropbox/notes'

filetype plugin indent on
syntax on
