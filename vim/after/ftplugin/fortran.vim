let s:extfname = expand("%:e")
if s:extfname ==? "f90"
    let fortran_free_source=1
    unlet! fortran_fixed_source
else
    let fortran_fixed_source=1
    unlet! fortran_free_source
endif 

setlocal expandtab
setlocal tabstop=2                   " An indentation every four columns
setlocal shiftwidth=2                " Use indents of 2 spaces
setlocal softtabstop=2               " Let backspace delete indent

let fortran_do_enddo=1
let fortran_more_precise=1
let fortran_have_tabs=1
