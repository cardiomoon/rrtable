#' Make ztable with desired width
#'@param df a data.frame
#'@param cwidth desired column width
#'@param width desired table width in column
#'@param ... further argument to be passed to ztable()
#'@export
ztable2=function(df,cwidth=NULL,width=80,...){

    if(is.null(cwidth)) cwidth=df2cwidth(df,width=width)
    align=cwidth2align(cwidth)
    df=HTMLcode2latex(df)
    z=ztable(df,align=align,include.rownames=FALSE,longtable=TRUE,...)
    z
}

cwidth2align=function(cwidth=NULL){
       result=c()
       for(i in seq_along(cwidth)){
           if(cwidth[i]<=0) {
               result=c(result,"l")
           } else{
               result=c(result,paste0("p{",cwidth[i],"cm}"))
           }
       }
       result=stringr::str_flatten(result,"")
       result
}

df2cwidth=function(data,width=80,min=10){
    current=apply(data,2,function(x) max(nchar(x)))
    preserveCol=current<min
    SumCurrent=sum(current[setdiff(1:ncol(data),preserveCol)])
    aim=current*width/SumCurrent
    aim[preserveCol]=current[preserveCol]
    aim[!preserveCol]=ifelse(aim[!preserveCol]<min,min,aim[!preserveCol])
    A=sum(aim[aim>min])
    B=width-sum(aim[aim<=min])
    aim[aim>min]=aim[aim>min]*B/A
    aim=round(aim) %/% 5
    aim
}

