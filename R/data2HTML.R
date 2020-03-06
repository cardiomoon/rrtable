#' Make a HTML5 file with a data.frame
#' @param data A data.frame
#' @param preprocessing A character string of R code
#' @param path A name of destination file path
#' @param filename A name of destination file
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @param type character "HTML" or "pdf"
#' @param vanilla logical. Whether or not make vanilla table
#' @param echo Logical. Whether or not show R code of plot and table
#' @param showself Logical. Whether or not show R code for the paragraph
#' @importFrom rmarkdown render
#' @importFrom moonBook mytable
#' @importFrom ztable ztable
#' @importFrom flextable width
#' @export
#' @examples
#' \donttest{
#' library(moonBook)
#' library(rrtable)
#' library(ggplot2)
#' data2HTML(sampleData2)
#' }
data2HTML=function(data,preprocessing="",path=".",filename="report.HTML",rawDataName=NULL,rawDataFile="rawData.RDS",
                   type="HTML",vanilla=FALSE,echo=TRUE,showself=FALSE){

    mode=0
    owd=getwd()
    if (is.null(path)) {
        path=tempdir()
        setwd(path)
        mode=1

    } else{
        if(!file.exists(path)) dir.create(path)
        path=paste0(owd,"/",path)
        setwd(path)
    }

    if((type=="pdf")&(filename=="report.HTML")) filename="report.pdf"
    if((type=="docx")&(filename=="report.HTML")) filename="report.docx"
    if((type=="pptx")&(filename=="report.HTML")) filename="report.pptx"

    file.create("report2.Rmd")
    tempReport <-  "report2.Rmd"

    if(ncol(data)==3) {
        shortdata=1
    } else {
        shortdata=0
    }

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
    if(mysubtitle!=""){
        mycat("subtitle: '",mysubtitle,"'\n")
    }
    mycat("author: '",myauthor,"'\n")
    mycat("date: '`r Sys.time()`'\n---\n")

    mycat("```{r setup, include=FALSE}\n")
    mycat("knitr::opts_chunk$set(echo =",echo,",message=FALSE,warning=FALSE,comment=NA,
          fig.width=9,fig.asp=0.618,fig.align='center',dpi=300,out.width='70%')\n")
    mycat("```\n")

    mycat("```{r,echo=",echo,",message=FALSE}\n")
    mycat("require(moonBook)\n")
    mycat("require(rrtable)\n")
    mycat("require(ggplot2)\n")
    mycat("require(webrSub)\n")
    mycat("require(ggthemes)\n")
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

    if(shiny::isRunning()){
      progress <- shiny::Progress$new()
      on.exit(progress$close())
      progress$set(message = "Making File", value = 0)
    } else{
      cat("Making File:")
    }

    for(i in 1:count){
      if(isRunning()){
        progress$inc(1/(nrow(data)), detail = paste("Doing part", i,"/",nrow(data)))
      } else(
        cat(i)
      )

        if(showself){

                mycat("\n\n")
                mycat("```{r}\n")
                mycat(paste0("df2flextable2(data[",i,",])\n"))
                mycat("```\n\n\n")

        }


        if(mypptlist$type[i] %in% c("##","###")){
            mycat(mypptlist$type[i],mypptlist$title[i],"\n\n")
        } else {
            if(mypptlist$title[i]!="") mycat("###",mypptlist$title[i],"\n\n")
        }


        if(shortdata==0){
            if(mypptlist$text[i]!="") mycat(mypptlist$text[i],"\n\n")
        }

        if(mypptlist$type[i]=="mytable") {
            mycat("```{r}\n")
            mycat("mytable2flextable(",mypptlist$code[i],",vanilla=",vanilla,")\n")
            mycat("```\n\n")

        } else if(mypptlist$type[i]=="data"){
            mycat("```{r}\n")
            mycat("df2flextable(",mypptlist$code[i],",vanilla=",vanilla,")\n")
            mycat("```\n\n")

        } else if(mypptlist$type[i]=="table") {
            mycat("```{r,results='asis'}\n")
            mycat(mypptlist$code[i],"\n")
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
        } else if(mypptlist$type[i]=="text") {

            mycat(mypptlist$code[i],'\n')

        } else if(mypptlist$code[i]!=""){
            mycat("```{r",ifelse(shortdata,"",mypptlist$option[i]),"}\n")
            mycat(mypptlist$code[i],'\n')
            mycat("```\n\n")
        }
        mycat("\n\n")

    }
    if(type=="HTML"){
       out <- rmarkdown::render('report2.Rmd', rmarkdown::html_document())
    }else if(type=="docx"){
      out <- rmarkdown::render('report2.Rmd', rmarkdown::word_document())
    } else if(type=="pptx"){
      out <- rmarkdown::render('report2.Rmd', rmarkdown::powerpoint_presentation())
    } else {
      out <- rmarkdown::render('report2.Rmd', rmarkdown::pdf_document())
    }

    result=file.rename(out, filename)
    #file.remove("report2.Rmd")
    setwd(owd)
    path=str_replace(path,"//","/")
    paste0(path,"/",filename)
}
