
## jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))

npreshape <- function(x,shape) {
    x<- array(x)
    ind <- which(shape ==  -1)
    shape[ind] = prod(dim(x))/prod(shape[-ind])
    return(array(x,shape))
}
