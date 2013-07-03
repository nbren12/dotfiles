npreshape <- function(x,shape) {
    x<- array(x)
    ind <- which(shape ==  -1)
    shape[ind] = prod(dim(x))/prod(shape[-ind])
    return(array(x,shape))
}
