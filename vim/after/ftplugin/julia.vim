set tags+=/Applications/Julia-0.3.0-rc2-3ddbaa1c03.app/Contents/Resources/julia/tags

function! IpythonJulia()
    let kern = system("find `cd ~; pwd`/.ipython -name '*.json' | grep julia")

    execute( "IPython " . kern)
endf
