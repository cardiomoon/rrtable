#' Concatenate to file
#' @param ... R object
#' @param file A connection
#' @export
mycat=function(...,file="report2.Rmd"){
    base::cat(...,file=file, append=TRUE)
}

#' Make a pdf file with a data.frame
#' @param data A data.frame
#' @param preprocessing A character string of R code
#' @param filename A path of destination file
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @param kotex Logical. Whether or not use kotex package of latex
#' @param echo Logical. Whether or not show R code of plot and table
#' @importFrom rmarkdown render
#' @importFrom stringr str_replace
#' @export
#' @examples
#' library(moonBook)
#' library(ztable)
#' data2pdf(sampleData3)
#' data2pdf(sampleData2)
data2pdf=function(data,preprocessing="",filename="report.pdf",rawDataName=NULL,rawDataFile="rawData.RDS",kotex=FALSE,echo=FALSE){

    # data=sampleData2[9,]
    # preprocessing="";filename="report.pdf";
    # rawDataName=NULL;rawDataFile="rawData.RDS";kotex=FALSE;echo=FALSE

    if(file.exists("report2.Rmd")) file.remove("report2.Rmd")
    tempReport <-  "report2.Rmd"

    if(ncol(data)==3) {
        shortdata=1
    } else {
        shortdata=0
    }

    data$code=str_replace_all(data$code,"df2flextable[1-9]?","ztable2")

    data$type=tolower(data$type)
    if("title" %in% data$type) {
        mytitle=data[data$type=="title",]$text[1]
        data=data[data$type!="title",]
    } else{
        mytitle="Web-based Analysis with R"
    }
    mysubtitle=""
    if("subtitle" %in% data$type) {
        mysubtitle=data[data$type=="subtitle",]$text[1]
        data=data[data$type!="subtitle",]
    }
    if("author" %in% data$type) {
        myauthor=data[data$type=="author",]$text[1]
        data=data[data$type!="author",]
    } else{
        myauthor="prepared by web-r.org"
    }



    mycat("---\ntitle: '",mytitle,"'\n")
    mycat("author: '",myauthor,"'\n")
    mycat("date: '`r Sys.time()`'\n")

    mycat("header-includes:\n")
    if(kotex==TRUE) mycat("- \\usepackage{kotex}\n")
    mycat("- \\usepackage{multirow}\n")
    mycat("- \\usepackage{colortbl}\n- \\usepackage{pdflscape}\n- \\usepackage[table]{xcolor}\n")
    mycat("- \\usepackage{tabularx,booktabs}\n- \\usepackage{boxedminipage}\n- \\usepackage{graphicx}\n")
    mycat("- \\usepackage{rotating}\n- \\usepackage{longtable}\n")
    mycat("---\n")
    mycat("```{r setup, include=FALSE}\n")
    mycat("knitr::opts_chunk$set(echo =",echo,",message=FALSE,warning=FALSE,comment=NA,
          fig.width=9,fig.asp=0.618,fig.align='center',out.width='70%')\n")
    mycat("```\n")

    mycat("```{r,echo=",echo,",message=FALSE}\n")
    mycat("require(moonBook)\n")
    mycat("require(xtable)\n")
    mycat("require(ztable)\n")
    mycat("require(rrtable)\n")
    mycat("require(ggplot2)\n")
    mycat("options(ztable.type='latex')\n")
    mycat("```\n\n")

    if(!is.null(rawDataName)){
        mycat("```{r}\n")
        mycat("# Read Raw Data\n")
        temp=paste0("rawData=readRDS('",rawDataFile,"')\n")
        mycat(temp)
        temp=paste0("assign('",rawDataName,"',rawData)\n")
        mycat(temp)
        mycat("```\n\n")
    }

    if(preprocessing!="") {
        eval(parse(text=preprocessing))
        mycat("```{r}\n")
        mycat("# Preprocessing\n")
        mycat(preprocessing,'\n')

        mycat("```\n\n")
    }

    mypptlist=data
    count=nrow(mypptlist)
    for(i in 1:count){



        if(mypptlist$type[i] == "header2"){
            mycat("##",mypptlist$title[i],"\n\n")
        } else if(mypptlist$type[i] == "header3"){
            mycat("###",mypptlist$title[i],"\n\n")
        } else {
            if(mypptlist$title[i]!="") mycat("###",mypptlist$title[i],"\n\n")
        }

        if(shortdata==0){
            if(mypptlist$text[i]!="") mycat(mypptlist$text[i],"\n\n")
        }

        if(mypptlist$type[i]=="table") {
            mycat("```{r,results='asis'}\n")
            temp=mypptlist$code[i]

            result<-eval(parse(text=temp))
            if("flextable" %in% class(result)){
                mycat("result=",mypptlist$code[i],"\n")
                mycat("df=result$body$dataset\n")
                mycat("df=html2latex(df)\n")
                mycat("class(df)='data.frame'\n")
                mycat("xtable(df)\n")
            } else {
                mycat(mypptlist$code[i],'\n')
            }
            mycat("```\n\n")

        } else if(mypptlist$type[i]=="mytable"){
            mycat("```{r,results='asis'}\n")
            mycat("result=",mypptlist$code[i],"\n")
            mycat("print(ztable(result,longtable=TRUE),type='latex')\n")
            mycat("```\n\n")
        } else if(mypptlist$type[i]=="data"){
            mycat("```{r,results='asis'}\n")
            mycat("xtable(",mypptlist$code[i],",auto=TRUE)\n")
            mycat("```\n\n")
        } else if(mypptlist$type[i]=="rcode") {
            mycat("```{r,echo=TRUE}\n")
            mycat(mypptlist$code[i],'\n')
            mycat("```\n\n")
        }  else if(mypptlist$type[i]=="code") {
            mycat("```{r,echo=TRUE,eval=FALSE}\n")
            mycat(mypptlist$code[i],'\n')
            mycat("```\n\n")
        } else if(mypptlist$type[i] %in% c("2ggplots","2plots")){
            mycat("```{r,out.width='50%',fig.align='default',fig.show='hold'}\n")
            mycat(mypptlist$code[i],'\n')
            mycat("```\n\n")
        } else if(mypptlist$type[i] =="text"){
            mycat(mypptlist$code[i],'\n')

        } else if(str_detect(mypptlist$code[i],"ztable")){
            mycat("```{r",ifelse(shortdata,"",mypptlist$option[i]),",results='asis'}\n")
            mycat(mypptlist$code[i],'\n')
            mycat("```\n\n")
        }  else if(mypptlist$code[i] !=""){
            mycat("```{r",ifelse(shortdata,"",mypptlist$option[i]),"}\n")
            mycat(mypptlist$code[i],'\n')
            mycat("```\n\n")
        }
        mycat("\n\n")

    }

    out <- rmarkdown::render('report2.Rmd', params=list(format="PDF"),rmarkdown::pdf_document())
    result=file.rename(out, filename)
    #file.remove("report2.Rmd")
    invisible(result)
}



#' Convert HTML table to latex table
#' @param data a data.frame
#' @export
HTMLcode2latex=function(data){
    seek=c("\\{","\\}","_","~")
    replace=c("\\\\{","\\\\}","\\\\_","\\\\~{}")
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

#'make latex table with ztable
#'@param data a data.frame
#'@param ... further argument to be passed to ztable
#'@export
#'@examples
#'ztable2(sampleData3)
ztable2=function(data,aim=NULL,type="latex",...){
    # data=sampleData3
    # aim=NULL
    data1 <- data %>%
        multiLineData() %>%
        adjustWidth(aim=aim) %>%
        lfData() %>%
        HTMLcode2latex() %>%
        as.data.frame()
    data1=data1[-ncol(data1)]
    print(ztable(data1,...),type=type,longtable=TRUE,include.rownames=FALSE)
}


#' adjust width of data.frame
#' @param data A data.frame
#' @param width total desired wdth
#' @param min minimum width of each column
#' @importFrom purrr map2_df
#' @export
adjustWidth=function(data,width=80,min=10,aim=NULL){

    if(is.null(aim)){
    current=apply(data,2,function(x) max(nchar(x)))
    preserveCol=current<min
    SumCurrent=sum(current[setdiff(1:ncol(data),preserveCol)])
    aim=current*width/SumCurrent
    aim[preserveCol]=current[preserveCol]
    aim[!preserveCol]=ifelse(aim[!preserveCol]<min,min,aim[!preserveCol])
    A=sum(aim[aim>min])
    B=width-sum(aim[aim<=min])
    aim[aim>min]=aim[aim>min]*B/A
    aim=round(aim)
    }
    map2_df(data,aim,tensiSplit2,exdent=0)
}

#' Split string vectors with desired length with exdent
#' @param x A string vector
#' @param ... further argument to be passed to tensiSplit
#' @export
tensiSplit2=function(x,...){
    result=c()
    for(i in 1:length(x)){
        temp=tensiSplit(x[i],...)
        temp=str_flatten(temp,"\n")
        result=c(result,temp)
    }
    result
}
