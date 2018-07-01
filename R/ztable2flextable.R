#' Convert ztable to flextable
#' @param z An object of classs ztable
#' @param ... Further argument to be passed to df2flextable
#' @return an object of class flextable
#' @export
ztable2flextable=function(z,...){
    df=z$x
    df2flextable(df,...)
}
