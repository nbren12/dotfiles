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
NeoBundle 'tpope/vim-repeat'
NeoBundle 'Lokaltog/vim-easymotion'
NeoBundle 'majutsushi/tagbar'

" Latex
NeoBundle 'git://git.code.sf.net/p/vim-latex/vim-latex'

" Python
NeoBundle 'davidhalter/jedi-vim'


" Testing plugins
NeoBundle 'scrooloose/syntastic'
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

" Run basic settings
ru vanilla.vim

" Plugin Settings

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
let g:neocomplete#use_vimproc = 1
" inoremap <expr><Tab>  neocomplete#start_manual_complete()
inoremap <expr><C-g>     neocomplete#undo_completion()


let g:neocomplete#force_omni_input_patterns = {}

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

" Jedi Settings
let g:jedi#popup_on_dot = 0
let g:jedi#popup_select_first =  0

autocmd FileType python setlocal omnifunc=jedi#completions
let g:jedi#completions_enabled = 0
let g:jedi#auto_vim_configuration = 0
" let g:neocomplete#force_omni_input_patterns.python = '\%([^. \t]\.\|^\s*@\|^\s*from\s.\+import \|^\s*from \|^\s*import \)\w*'
" alternative pattern: '\h\w*\|[^. \t]\.\w*'

colo wombat

