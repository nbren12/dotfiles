setlocal wrap
setlocal spell
setlocal nocursorline
nmap <LocalLeader>ll :Latexmk<CR>
iab deg <BS>$^{\circ}$
iab wihtout without
iab waht what
iab <buffer> ,b \begin{
iab <buffer> ,e \end{


let g:neocomplete#disable_auto_complete = 1
set iskeyword+=;,-
let g:tex_flavor = "latex"

let b:tex_flavor = 'pdflatex'

nnoremap <F5> :make  <CR>
nnoremap <F6> execute ":make view" <CR>
