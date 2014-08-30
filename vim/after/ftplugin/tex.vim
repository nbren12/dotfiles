setlocal wrap
setlocal spell
setlocal nocursorline

iab deg <BS>$^{\circ}$
iab wihtout without
iab waht what
iab <buffer> ,b \begin{
iab <buffer> ,e \end{
iab `a \alpha
iab `b \beta
iab `q \theta


let g:neocomplete#disable_auto_complete = 1
set iskeyword+=;,-
let g:tex_flavor = "latex"

let b:tex_flavor = 'pdflatex'
if has('mac') 
    let g:pdf_viewer = "open"
else
    let g:pdf_viewer =  "evince"
endif


function! Gettexmain()
    return substitute(glob('*.latexmain'), ".latexmain", "", "g")
endfunction

silent execute "setlocal makeprg=rubber-info\\ ".Gettexmain()

function! Compilepdf()
    echom "Compling PDF"
    call system("rubber --pdf -f ". Gettexmain()." &")
endf

function! Viewtexmain()
    let b:pdfmain = substitute(Gettexmain(), ".tex", ".pdf", "g")
    call system( g:pdf_viewer." ". b:pdfmain ." &")
endf

function! Viewerror()
    silent make 
    copen 
endf



map <Plug>(make-pdf)   :call Compilepdf() <CR>
map <Plug>(view-pdf)   :call Viewtexmain() <CR>
map <Plug>(view-error) :call Viewerror() <CR>
nmap <silent> <Leader>ll <Plug>(make-pdf)
nmap <silent> <Leader>lv <Plug>(view-pdf)
nmap <silent> <Leader>le <Plug>(view-error)

