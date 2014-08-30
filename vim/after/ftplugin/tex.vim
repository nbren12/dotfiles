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
iab txt \text{


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

function! Getpdfmain()
    return substitute(Gettexmain(), ".tex", ".pdf", "g")
endfunction


let b:pdfmain = Getpdfmain()

silent execute "setlocal makeprg=rubber-info\\ ".Gettexmain()

function! Compilepdf()
    echom "Compling PDF"
    call system("latexmk -latexoption='-synctex=1' --silent --pdf -f ". Gettexmain()." &")
endf

function! Viewerror()
    silent make 
    copen 
endf

function! Viewtexmain()
    call system( g:pdf_viewer." ". b:pdfmain ." &")
endf

function! ForwardSearchViewer()
    if has('mac')
        let cmd = "/Applications/Skim.app/Contents/SharedSupport/displayline ". line('.') 
                    \." ". Getpdfmain() ." ". expand("%:p")
    else
        let cmd = 'evince_forward_search ' 
                    \. Getpdfmain() ." ". line(".") . " ".expand("%:p") 
    endif
    call system(cmd)
endf

map <silent> <Leader>ls   :call ForwardSearchViewer() <CR>
map <Plug>(make-pdf)   :call Compilepdf() <CR>
map <Plug>(view-pdf)   :call Viewtexmain() <CR>
map <Plug>(view-error) :call Viewerror() <CR>
nmap <silent> <Leader>ll <Plug>(make-pdf)
nmap <silent> <Leader>lv <Plug>(view-pdf)
nmap <silent> <Leader>le <Plug>(view-error)

