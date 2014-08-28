setlocal wrap
setlocal spell
nmap <LocalLeader>ll :Latexmk<CR>
iab deg <BS>$^{\circ}$
iab wihtout without
iab waht what
iab <buffer> ,b \begin{
iab <buffer> ,e \end{


let g:neocomplete#disable_auto_complete = 1
set iskeyword+=;,-
let g:tex_flavor = "latex"

