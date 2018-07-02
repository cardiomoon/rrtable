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
#' @param path A name of destination file path
#' @param filename A path of destination file
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @param kotex Logical. Whether or not use kotex package of latex
#' @param echo Logical. Whether or not show R code of plot and table
#' @param showself Logical. Whether or not show R code for the paragraph
#' @importFrom rmarkdown render
#' @importFrom stringr str_replace
#' @export
#' @examples
#' library(moonBook)
#' library(ztable)
#' library(ggplot2)
#' \donttest{
#' data2pdf(sampleData2,path="tmp")
#' }
data2pdf=function(data,preprocessing="",path=NULL,filename="report.pdf",rawDataName=NULL,
                  rawDataFile="rawData.RDS",kotex=FALSE,echo=TRUE,showself=FALSE){

    # data=sampleData2[9,]
    # preprocessing="";filename="report.pdf";
    # rawDataName=NULL;rawDataFile="rawData.RDS";kotex=FALSE;echo=FALSE

    mode=0
    owd=getwd()
    # cat("owd=",owd,"\n")
    if (is.null(path)) {
        path=tempdir()
        setwd(path)
        mode=1

    } else{
        if(!file.exists(path)) dir.create(path)
        path=paste0(owd,"/",path)
        setwd(path)
    }

    file.create("report2.Rmd")

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
        if(shortdata) mytitle=data[data$type=="title",]$code[1]
        data=data[data$type!="title",]
    } else{
        mytitle="Web-based Analysis with R"
    }
    mysubtitle=""
    if("subtitle" %in% data$type) {
        mysubtitle=data[data$type=="subtitle",]$text[1]
        if(shortdata) mysubtitle=data[data$type=="subtitle",]$code[1]
        data=data[data$type!="subtitle",]
    }
    if("author" %in% data$type) {
        myauthor=data[data$type=="author",]$text[1]
        if(shortdata) myauthor=data[data$type=="author",]$code[1]
        data=data[data$type!="author",]
    } else{
        myauthor="prepared by web-r.org"
    }



    mycat("---\ntitle: '",mytitle,"'\n")
    mycat("subtitle: '",mysubtitle,"'\n")
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

        if(showself){

                mycat("\n\n")
                mycat("```{r,results='asis',echo=FALSE}\n")

                mycat("ztable2(data[",i,",])\n")
                mycat("```\n\n\n")

        }


        landscape=FALSE
        if(mypptlist$type[i] == "mytable"){
            mycat("\n\\newpage\n\n")
        }
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

        # if(mypptlist$type[i]=="table") {
        #     mycat("```{r,results='asis'}\n")
        #     temp=mypptlist$code[i]
        #
        #     result<-eval(parse(text=temp))
        #     if("flextable" %in% class(result)){
        #         mycat("result=",mypptlist$code[i],"\n")
        #         mycat("df=result$body$dataset\n")
        #         mycat("df=html2latex(df)\n")
        #         mycat("class(df)='data.frame'\n")
        #         mycat("xtable(df)\n")
        #     } else {
        #         mycat(mypptlist$code[i],'\n')
        #     }
        #     mycat("```\n\n")
        #
        # } else
        if(mypptlist$type[i]=="mytable"){
            mycat("```{r,results='asis'}\n")
            mycat("result=",mypptlist$code[i],"\n")
            mycat("print(ztable(result,longtable=TRUE),type='latex')\n")
            mycat("```\n\n")
        } else if(mypptlist$type[i]=="data"){
            mycat("```{r,results='asis'}\n")
            mycat("ztable2(",mypptlist$code[i],")\n")
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
            if(landscape==TRUE){
                temp=mypptlist$code[i]
                mycat(temp,'\n')
            } else{
                mycat(mypptlist$code[i],'\n')
            }
            mycat("```\n\n")
        }  else if(mypptlist$type[i] %in% c("table","Table")){
            result=eval(parse(text=mypptlist$code[i]))
            if("flextable" %in% class(result)){
                mycat("```{r",ifelse(shortdata,"",mypptlist$option[i]),",results='asis'}\n")
                mycat(paste0('flextable2ztable(',mypptlist$code[i],')\n'))
            } else{
                mycat("```{r",ifelse(shortdata,"",mypptlist$option[i]),"}\n")
                mycat(mypptlist$code[i],'\n')
            }
            mycat("```\n\n")
        } else if(mypptlist$code[i] !=""){

               mycat("```{r",ifelse(shortdata,"",mypptlist$option[i]),"}\n")
               mycat(mypptlist$code[i],'\n')
              mycat("```\n\n")
        }
        mycat("\n\n")

    }

    out <- rmarkdown::render('report2.Rmd', params=list(format="PDF"),rmarkdown::pdf_document())
    result=file.rename(out, filename)
    #file.remove("report2.Rmd")
    setwd(owd)
    if(mode) result=file.copy(paste0(path,"/",filename),filename,overwrite=TRUE)
    path=str_replace(path,"//","/")
    paste0(path,"/",filename)
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
