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

" Plugins {{{


    set rtp+=~/.vim/bundle/vundle/
    call vundle#rc()
    Bundle 'gmarik/vundle' 

    Bundle 'hsitz/VimOrganizer'
    let g:org_command_for_emacsclient = "/usr/local/bin/emacsclient"

    Bundle 'vim-pandoc/vim-pandoc'
    Bundle 'Vim-R-plugin'
    Bundle 'vim-scripts/MatlabFilesEdition'
    Bundle 'https://github.com/tpope/vim-fugitive.git' 
    Bundle 'mileszs/ack.vim'
    Bundle 'taglist.vim'
    Bundle 'tComment'
    Bundle 'surround.vim'
    Bundle 'mattn/calendar-vim'
    Bundle 'vim-scripts/utl.vim'
    Bundle 'scrooloose/nerdtree'
    map <C-e> :NERDTreeToggle<CR>
    let NERDTreeShowBookmarks=1

    " ctrlp {{{
    Bundle 'ctrlp.vim'
    nnoremap <silent> <D-t> :CtrlP<CR>
    nnoremap <silent> <D-r> :CtrlPMRU<CR>
    nnoremap <silent> <Leader>r :CtrlPMRU<CR>
    nnoremap <silent> <Leader>b :CtrlPBuffer<CR>
    let g:ctrlp_custom_ignore = {
        \ 'dir':  '\.git$\|\.hg$\|\.svn$',
        \ 'file': '\.swp$|\.exe$\|\.so$\|\.dll$' }

    let g:ctrlp_lazy_update = 1
    let g:ctrlp_max_depth = 5
    let g:ctrlp_working_path_mode = 2
    " let g:ctrlp_working_path_mode='r'
    let g:ctrlp_user_command = {
                \ 'types': {
                \ 1: ['.git', 'cd %s && git ls-files'],
                \ 2: ['.hg', 'hg --cwd %s locate -I .'],
                \ },
                \ 'fallback': ' find %s -type f' }
                
    let g:ctrlp_root_markers = ['.ctrlp','.git']
    "}}}
    "Code completion"{{{
    Bundle 'ervandew/supertab'
    let g:SuperTabDefaultCompletionType = "context"
    set completeopt+=longest
    
    
    Bundle 'davidhalter/jedi-vim'
    let g:jedi#goto_assignments_command = "<leader>g"
    let g:jedi#goto_definitions_command = "<leader>d"
    let g:jedi#documentation_command = "K"
    let g:jedi#usages_command = "<leader>n"
    let g:jedi#completions_command = "<C-Space>"
    let g:jedi#rename_command = ""


    " Bundle 'neocomplcache'
    " let g:neocomplcache_enable_at_startup = 1 
        
    if has('mac')
        " Bundle "Valloric/YouCompleteMe"
        " let g:ycm_autoclose_preview_window_after_insertion=1
        " let g:ycm_key_list_previous_completion=['<Up>']
    else
        " Bundle 'neocomplcache'
        " let g:neocomplcache_enable_at_startup = 1 
    endif 



    "" Snippets: 
    " Sat Aug 10 01:52:15 EDT 2013: UltiSnips is too slow
    " Bundle "garbas/vim-snipmate"
    Bundle 'UltiSnips'
    let g:UltiSnipsExpandTrigger="<s-tab>"
    let g:UltiSnipsListSnippets="<c-tab>"
    let g:UltiSnipsSnippetDirectories=["UltiSnips", "mysnippets"]
    "}}}

    au! BufRead,BufWrite,BufWritePost,BufNewFile *.org
    au BufEnter *.org call OrgOpts()

    function OrgOpts()
        call org#SetOrgFileType()
        unmap gj
        unmap gk
    endfunction



    au BufRead,BufNewFile *.md  set filetype=pandoc

    
    " Autoclosing 
    " Tried many plugins, Townk is bad, AutoClose is bad
    " http://stackoverflow.com/questions/883437/how-do-i-get-vim-to-automatically-put-ending-braces/883522#883522
    
    Bundle "jiangmiao/auto-pairs"
    
    " Colorschemes"{{{
    Bundle 'Wombat'
    Bundle 'Lokaltog/vim-distinguished'
    Bundle 'jellybeans.vim'
    Bundle 'zeis/vim-kolor'
    "}}}

    Bundle 'laktek/distraction-free-writing-vim'
    Bundle 'jcf/vim-latex'
    let g:Tex_ViewRule_pdf = 'Skim'
    let g:tex_flavor='latex'
    " let g:Tex_TreatMacViewerAsUNIX = 1
    set grepprg=grep\ -nH\ $*
    imap ;; <Plug>IMAP_JumpForward
    "Fix the C-j nonsense in non latex files"
    if &filetype != 'tex'
        imap <Leader>#$ <Plug>IMAP_JumpForward
        nmap <Leader>#$ <Plug>IMAP_JumpForward
        vmap <Leader>#$ <Plug>IMAP_JumpForward
        vmap <Leader>#$ <Plug>IMAP_DeleteAndJumpForward
    endif
    
    " For matching in fortran"
    Bundle 'matchit.zip'

    Bundle 'vim-ipython'
    let g:ipy_completefunc = 'local'

" }}}

" User Interface {{{
    set hlsearch
    set viewoptions=folds,options,cursor,unix,slash " Better Unix / Windows compatibility
    syntax on

    if has("gui_running")
        colorscheme wombat
        
        "set guifont=Mono\ Regular:h14,Menlo\ Regular:h14,Consolas\ Regular:h14,Courier\ New\ Regular:h16
        set guifont=Monospace\ 10,Consolas:h13
        " turn off the toolbar
        set guioptions-=T
    else 
        " colorscheme desert 
    endif

    "Folding "
    let fortran_fold=1
    let fortran_fold_conditionals=1
    set foldmethod=marker
    set wrap " word wrap
    autocmd FileType c,matlab,cpp,fortran setlocal foldmethod=syntax
    autocmd FileType python setlocal foldmethod=indent

    " This is why autocomplete is abysmally slow sometimes
    " Don't screw up folds when inserting text that might affect them, until
    " leaving insert mode. Foldmethod is local to the window. Protect against
    " screwing up folding when switching between windows.
    autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
    autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif

    " Accept Mouse Input
    set mouse=a

    set number
    let g:fullscreen_colorscheme = "iawriter"
    let g:fullscreen_font = "DejaVu\ Sans\ Mono\ Book\ 12"
    let g:normal_colorscheme = "default"
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
    nnoremap <Leader>c <C-w>c
    nnoremap <Leader>o <C-w>o


    "Insert mode movement
    imap <C-a> <C-O>0
    imap <C-e> <C-O>$


    " Movement
     " mapping to make movements operate on 1 screen line in wrap mode
     function! ScreenMovement(movement)
         if &wrap
             return "g" . a:movement
         else
             return a:movement
         endif
     endfunction
     onoremap <silent> <expr> j ScreenMovement("j")
     onoremap <silent> <expr> k ScreenMovement("k")
     onoremap <silent> <expr> 0 ScreenMovement("0")
     onoremap <silent> <expr> ^ ScreenMovement("^")
     onoremap <silent> <expr> $ ScreenMovement("$")
     nnoremap <silent> <expr> j ScreenMovement("j")
     nnoremap <silent> <expr> k ScreenMovement("k")
     nnoremap <silent> <expr> 0 ScreenMovement("0")
     nnoremap <silent> <expr> ^ ScreenMovement("^")
     nnoremap <silent> <expr> $ ScreenMovement("$")

     " Ctags Code browsing shortcuts
     map <C-\> :tab split<CR>:exec("tag ".expand("<cword>"))<CR>
     map <A-]> :vsp <CR>:exec("tag ".expand("<cword>"))<CR>

     nnoremap <Leader>t :TlistToggle<CR>
    nnoremap yg "+y
    vnoremap yg "+y
    
    map <Leader>5 <F5>

    " Fugitive"
    nnoremap <silent> <Leader>g  :Gstatus<CR>
    " Folding 
    nnoremap <space> za
    vnoremap <space> zf
    set foldnestmax=2


    " Date Time
    iab <expr> dts strftime("%b %d, %Y")


"}}}

"  Programming {{{
    " CTags Browsing
    set tags=./tags;/
    " nmap <S-F> :set syntax=fortran<CR>:let b:fortran_fixed_source=!b:fortran_fixed_source<CR>:set syntax=text<CR>:set syntax=fortran<CR>
    let fortran_do_enddo=1
    let fortran_more_precise=1
    " let fortran_free_source=1
    " let fortran_have_tabs=1

    " Fix Indenting Behavior with #
    " inoremap # X#   

    if has("autocmd") && exists("+omnifunc")
	autocmd Filetype *
		    \	if &omnifunc == "" |
		    \		setlocal omnifunc=syntaxcomplete#Complete |
		    \	endif
    endif
"}}}

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

set listchars=""

set wildignore+=*.so,*.swp,*.zip
set wildignore+=*.Trash/*

set autochdir

filetype plugin indent on
syntax on
