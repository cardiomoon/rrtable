#' coerce an object of type "numeric"
#' @param x A vector
#' @export
#' @examples
#' x=c("1,200","2","3.5")
#' x=factor(3:1)
#' x=c(1:3,"tt")
#' as.mynumeric(x)
as.mynumeric=function(x){
    if(is.factor(x)) {
        temp=as.character(x)
    } else{
        temp=x
    }
    temp=gsub(",","",temp)
    res<-tryCatch(as.numeric(temp),warning=function(w) "warning")
    if(inherits(res,"numeric")) {
        res
    } else{
        x
    }
}


#' Make flextable with a data.frame
#' @param df A data.frame
#' @param numericCol Numeric. Columns to be treated as numeric
#' @export
#' @importFrom flextable flextable delete_part add_header_row theme_booktabs align_nottext_col autofit
myFlextable=function(df,numericCol=NULL){
    if(!is.null(numericCol)){
        for(i in seq_along(numericCol)) {
            df[[numericCol[i]]]=as.numeric(df[[numericCol[i]]])
        }
    } else{
        df[]=lapply(df,as.mynumeric)
    }
    if(sum(duplicated(names(df)))>0) {
        labels=colnames(df)
        colnames(df)=paste0("col",1:ncol(df))
        ft=flextable(df)
        ft<-ft %>% delete_part(part="header") %>% add_header_row(values=labels) %>% theme_booktabs()
    } else{
        ft=flextable(df)
    }
    ft %>% align_nottext_col(align="right") %>% autofit()
}
