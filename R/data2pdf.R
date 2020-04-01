#' Concatenate to file
#' @param ... R object
#' @param file A connection
#' @export
mycat=function(...,file="report2.Rmd"){
    base::cat(...,file=file, append=TRUE)
}

#' Make a pdf file with a data.frame
#' @param ... further arguments to be passed to data2HTML
#' @importFrom rmarkdown render
#' @export
#' @examples
#' library(moonBook)
#' library(ztable)
#' library(ggplot2)
#' \donttest{
#' data2pdf(sampleData2)
#' }
data2pdf=function(...){

    # data=sampleData2[9,]
    # preprocessing="";filename="report.pdf";
    # rawDataName=NULL;rawDataFile="rawData.RDS";kotex=FALSE;echo=FALSE

    data2HTML(...,type="pdf")
}

#' Make a word file with a data.frame
#' @param ... further arguments to be passed to data2HTML
#' @importFrom rmarkdown render
#' @export
data2docx2=function(...){

    # data=sampleData2[9,]
    # preprocessing="";filename="report.pdf";
    # rawDataName=NULL;rawDataFile="rawData.RDS";kotex=FALSE;echo=FALSE

    data2HTML(...,type="docx")
}

#' Make a powerpoint file with a data.frame
#' @param ... further arguments to be passed to data2HTML
#' @importFrom rmarkdown render
#' @export
data2pptx2=function(...){

    # data=sampleData2[9,]
    # preprocessing="";filename="report.pdf";
    # rawDataName=NULL;rawDataFile="rawData.RDS";kotex=FALSE;echo=FALSE

    data2HTML(...,type="pptx")
}



#' Convert HTML table to latex table
#' @param data a data.frame
#' @export
HTMLcode2latex=function(data){
    seek=c("\\{","\\}","_","~","\n")
    replace=c("\\\\{","\\\\}","\\\\_","\\\\~{}","\\\\newline ")
    code=data.frame(seek,replace,stringsAsFactors = FALSE)

    fnr=function(x){

        for(i in 1:nrow(code)){
            x=stringr::str_replace_all(x,code$seek[i],code$replace[i])
        }
        x
    }

    data[]=lapply(data,fnr)
    data
}
