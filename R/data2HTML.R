#' Make a HTML5 file with a data.frame
#' @param data A data.frame
#' @param preprocessing A character string of R code
#' @param filename A path of destination file
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @param vanilla logical. Whether or not make vanilla table
#' @param echo Logical. Whether or not show R code of plot and table
#' @importFrom rmarkdown render
#' @importFrom moonBook mytable
#' @importFrom ztable ztable print.ztable
#' @export
#' @examples
#' library(moonBook)
#' library(ztable)
#' library(rrtable)
#' data2HTML(sampleData3)
data2HTML=function(data,preprocessing="",filename="report.HTML",rawDataName=NULL,rawDataFile="rawData.RDS",
                   vanilla=FALSE,echo=FALSE){


    if(file.exists("report2.Rmd")) file.remove("report2.Rmd")

    tempReport <-  "report2.Rmd"

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
    if(mysubtitle!=""){
        mycat("subtitle: '",mysubtitle,"'\n")
    }
    mycat("author: '",myauthor,"'\n")
    mycat("date: '`r Sys.time()`'\n---\n")

    mycat("```{r setup, include=FALSE}\n")
    mycat("knitr::opts_chunk$set(echo =",echo,",message=FALSE,warning=FALSE,comment=NA,
          fig.width=9,fig.asp=0.618,fig.align='center',out.width='70%')\n")
    mycat("```\n")

    mycat("```{r,echo=",echo,",message=FALSE}\n")
    mycat("require(moonBook)\n")
    mycat("require(ztable)\n")
    mycat("require(rrtable)\n")
    mycat("require(ggplot2)\n")
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
        mycat("```{r}\n")
        mycat("# Preprocessing\n")
        mycat(preprocessing,'\n')
        mycat("```\n\n")
    }

    mypptlist=data
    count=nrow(mypptlist)
    for(i in 1:count){

        if(mypptlist$type[i] %in% c("##","###")){
            mycat(mypptlist$type[i],mypptlist$title[i],"\n\n")
        } else {
            if(mypptlist$title[i]!="") mycat("###",mypptlist$title[i],"\n\n")
        }

        if(mypptlist$text[i]!="") mycat(mypptlist$text[i],"\n\n")

        if(mypptlist$type[i]=="mytable") {
            mycat("```{r,results='asis'}\n")
            mycat("mytable2flextable(",mypptlist$code[i],",vanilla=",vanilla,")\n")
            mycat("```\n\n")

        } else if(mypptlist$type[i]=="data"){
            mycat("```{r,results='asis'}\n")
            mycat("df2flextable(",mypptlist$code[i],",vanilla=",vanilla,")\n")
            mycat("```\n\n")

        } else if(mypptlist$type[i]=="table") {
            mycat("```{r,results='asis'}\n")
            code=set_argument(mypptlist$code[i],argument="vanilla",value=vanilla)
            mycat(code,"\n")
            mycat("```\n\n")
        } else if(mypptlist$type[i]=="rcode") {
            mycat("```{r,echo=TRUE}\n")
            mycat(mypptlist$code[i],'\n')
            mycat("```\n\n")
        } else if(mypptlist$type[i]=="code") {
            mycat("```{r,echo=TRUE,eval=FALSE}\n")
            mycat(mypptlist$code[i],'\n')
            mycat("```\n\n")
        } else if(mypptlist$type[i]=="eval") {
            mycat("```{r}\n")
            mycat(mypptlist$code[i],'\n')
            mycat("```\n\n")
        } else if(mypptlist$type[i] %in% c("2ggplots","2plots")){
            mycat("```{r,out.width='50%',fig.align='default',fig.show='hold'}\n")
            mycat(mypptlist$code[i],'\n')
            mycat("```\n\n")
        } else if(mypptlist$code[i]!=""){
            mycat("```{r",mypptlist$option[i],"}\n")
            mycat(mypptlist$code[i],'\n')
            mycat("```\n\n")
        }
        mycat("\n\n")

    }

    out <- rmarkdown::render('report2.Rmd', rmarkdown::html_document())
    result=file.rename(out, filename)
    #file.remove("report2.Rmd")
    invisible(result)
}
