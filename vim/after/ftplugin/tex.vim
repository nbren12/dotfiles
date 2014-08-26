setlocal iskeyword+=:,-
setlocal makeprg=pdflatex\ -file-line-error\ -interaction=nonstopmode\ %
setlocal wrap

inoremap <buffer> { {}<ESC>i
inoremap <buffer> [ []<ESC>i

iab <buffer> ,b \begin{
iab <buffer> ,e \end{
" More abbreviations...



imap <buffer> [[     \begin{
imap <buffer> ((     \eqref{



