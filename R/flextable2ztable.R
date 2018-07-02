#' Convert flextable to ztable
#' @param ft An object of classs flextable
#' @param ... Further argument to be passed to ztable
#' @return an object of class ztable
#' @export
flextable2ztable=function(ft,...){
    df=ft$body$dataset
    ztable2(df,longtable=TRUE,...)
}
