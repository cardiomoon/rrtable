#' Convert flextable to ztable
#' @param ft An object of class flextable
#' @param type  "html" or "latex"
#' @param ... Further argument to be passed to ztable
#' @return an object of class ztable
#' @export
flextable2ztable=function(ft,type="html"){
    df=ft$body$dataset
    colnames(df)=stringr::str_replace_all(colnames(df),stringr::fixed("%"),stringr::fixed("\\%"))
    colnames(df)=stringr::str_replace_all(colnames(df),stringr::fixed("\U03B2"),stringr::fixed("$\\beta$"))
    z=ztable(df,longtable=TRUE)
    if(type=="latex"){
        print(z,type="latex")
    } else {z}
}
