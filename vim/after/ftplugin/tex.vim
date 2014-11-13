setlocal wrap
setlocal spell
setlocal nocursorline
setlocal lbr

setlocal iskeyword+=:,-,_
set sw=2

let g:tex_fold_enabled = 0
let g:neocomplete#disable_auto_complete = 1

iab deg <BS>$^{\circ}$
iab wihtout without
iab waht what
iab <buffer> ,b \begin{
iab <buffer> ,e \end{
" iab `a \alpha
" iab `b \beta
" iab `q \theta
iab txt \text{

" Latex-Suite
" imap <C-B> <Plug>Tex_MathBF
" imap <C-C> <Plug>Tex_MathCal
" imap  <C-t> <Plug>Tex_LeftRight
" imap <C-I> <Plug>Tex_InsertItemOnThisLine

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"               Greek letters and keybindings from auctex.vim                "
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

" Greek letters, AucTex style bindings {{{
" No timeout. Applies to mappings such as `a for \alpha
if 0 
set notimeout
inoremap <buffer> <LocalLeader><LocalLeader> <LocalLeader>
inoremap <buffer> <LocalLeader>a \alpha
inoremap <buffer> <LocalLeader>b \beta
inoremap <buffer> <LocalLeader>c \chi
inoremap <buffer> <LocalLeader>d \delta
inoremap <buffer> <LocalLeader>e \epsilon
" inoremap <buffer> <LocalLeader>e \varepsilon
inoremap <buffer> <LocalLeader>f \phi
" inoremap <buffer> <LocalLeader>f \varphi
inoremap <buffer> <LocalLeader>g \gamma
inoremap <buffer> <LocalLeader>h \eta
inoremap <buffer> <LocalLeader>i \int_{}^{}<Esc>F}i
" Or \iota or \infty or \in
inoremap <buffer> <LocalLeader>k \kappa
inoremap <buffer> <LocalLeader>l \lambda
inoremap <buffer> <LocalLeader>m \mu
inoremap <buffer> <LocalLeader>n \nu
inoremap <buffer> <LocalLeader>o \omega
inoremap <buffer> <LocalLeader>p \pi
inoremap <buffer> <LocalLeader>q \theta
inoremap <buffer> <LocalLeader>r \rho
inoremap <buffer> <LocalLeader>s \sigma
inoremap <buffer> <LocalLeader>t \tau
inoremap <buffer> <LocalLeader>u \upsilon
inoremap <buffer> <LocalLeader>v \vee
inoremap <buffer> <LocalLeader>w \wedge
inoremap <buffer> <LocalLeader>x \xi
inoremap <buffer> <LocalLeader>y \psi
inoremap <buffer> <LocalLeader>z \zeta
inoremap <buffer> <LocalLeader>D \Delta
inoremap <buffer> <LocalLeader>I \int_{}^{}<Esc>F}i
inoremap <buffer> <LocalLeader>F \Phi
inoremap <buffer> <LocalLeader>G \Gamma
inoremap <buffer> <LocalLeader>L \Lambda
inoremap <buffer> <LocalLeader>N \nabla
inoremap <buffer> <LocalLeader>O \Omega
inoremap <buffer> <LocalLeader>Q \Theta
inoremap <buffer> <LocalLeader>R \varrho
inoremap <buffer> <LocalLeader>S \sum_{}^{}<Esc>F}i
inoremap <buffer> <LocalLeader>U \Upsilon
inoremap <buffer> <LocalLeader>X \Xi
inoremap <buffer> <LocalLeader>Y \Psi
inoremap <buffer> <LocalLeader>0 \emptyset
inoremap <buffer> <LocalLeader>1 \left
inoremap <buffer> <LocalLeader>2 \right
inoremap <buffer> <LocalLeader>3 \Big
inoremap <buffer> <LocalLeader>6 \partial
inoremap <buffer> <LocalLeader>8 \infty
inoremap <buffer> <LocalLeader>/ \frac{}{}<Esc>F}i
inoremap <buffer> <LocalLeader>% \frac{}{}<Esc>F}i
inoremap <buffer> <LocalLeader>@ \circ
inoremap <buffer> <LocalLeader>\| \Big\|
inoremap <buffer> <LocalLeader>= \equiv
inoremap <buffer> <LocalLeader>\ \setminus
inoremap <buffer> <LocalLeader>. \cdot
inoremap <buffer> <LocalLeader>* \times
inoremap <buffer> <LocalLeader>& \wedge
inoremap <buffer> <LocalLeader>- \bigcap
inoremap <buffer> <LocalLeader>+ \bigcup
inoremap <buffer> <LocalLeader>( \subset
inoremap <buffer> <LocalLeader>) \supset
inoremap <buffer> <LocalLeader>< \leq
inoremap <buffer> <LocalLeader>> \geq
inoremap <buffer> <LocalLeader>, \nonumber
inoremap <buffer> <LocalLeader>: \dots
inoremap <buffer> <LocalLeader>~ \tilde{}<Left>
inoremap <buffer> <LocalLeader>^ \hat{}<Left>
inoremap <buffer> <LocalLeader>; \dot{}<Left>
inoremap <buffer> <LocalLeader>_ \bar{}<Left>
inoremap <buffer> <LocalLeader><M-c> \cos
inoremap <buffer> <LocalLeader><C-E> \exp\left(\right)<Esc>F(a
inoremap <buffer> <LocalLeader><C-I> \in
inoremap <buffer> <LocalLeader><C-J> \downarrow
inoremap <buffer> <LocalLeader><C-L> \log
inoremap <buffer> <LocalLeader><C-P> \uparrow
inoremap <buffer> <LocalLeader><Up> \uparrow
inoremap <buffer> <LocalLeader><C-N> \downarrow
inoremap <buffer> <LocalLeader><Down> \downarrow
inoremap <buffer> <LocalLeader><C-F> \to
inoremap <buffer> <LocalLeader><Right> \lim_{}<Left>
inoremap <buffer> <LocalLeader><C-S> \sin
inoremap <buffer> <LocalLeader><C-T> \tan
inoremap <buffer> <LocalLeader><M-l> \ell
inoremap <buffer> <LocalLeader><CR> \nonumber\\<CR><HOME>&&<Left>
endif
" }}}


map <silent> <Leader>ls :silent
        \ !/Applications/Skim.app/Contents/SharedSupport/displayline
        \ <C-R>=line('.')<CR> "<C-R>=LatexBox_GetOutputFile()<CR>"
        \ "%:p" <CR>

" My own list of commands
if 0 
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
    echom "Compiling PDF"
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

endif
