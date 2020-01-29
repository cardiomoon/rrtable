#' convert data to pptx file
#' @param data A document object
#' @param preprocessing A string
#' @param path A name of destination file path
#' @param filename File name
#' @param format desired format. choices are "pptx" or "docx"
#' @param width the width of the device.
#' @param height the height of the device.
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer. Also used for units other than the default, and to convert points to pixels.
#' @param rawDataName raw Data Name
#' @param rawDataFile raw Data File
#' @param vanilla logical. Whether or not make vanilla table
#' @param echo logical Whether or not show R code
#' @param landscape Logical. Whether or not make a landscape section.
#' @param showself Logical. Whether or not show R code for the paragraph
#' @importFrom officer read_docx read_pptx
#' @importFrom ztable ztable2flextable
#' @export
data2office=function(data,
                     preprocessing="",
                     path=".",filename="Report",format="pptx",width=7,height=5,units="in",
                     res=300,rawDataName=NULL,rawDataFile="rawData.RDS",vanilla=FALSE,echo=FALSE,
                     landscape=FALSE,
                     showself=FALSE){

    # path=".";filename="Report";format="pptx";width=7;height=5;units="in"
    # res=300;rawDataName=NULL;rawDataFile="rawData.RDS";vanilla=FALSE;echo=FALSE
    # landscape=FALSE;
    # showself=FALSE

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

    if(!is.null(rawDataName)){
        rawData=readRDS(rawDataFile)
        assign(rawDataName,rawData)
    }

    if(preprocessing!="") {

        eval(parse(text=preprocessing))
    }
    data$type=tolower(data$type)

    if(ncol(data)==3) {
        shortdata=1
    } else {
        shortdata=0
    }

    if("title" %in% data$type) {
        if(shortdata) {
            mytitle=data[data$type=="title",]$code[1]
        } else{
            mytitle=data[data$type=="title",]$text[1]
        }
        data=data[data$type!="title",]
    } else{
        mytitle="Web-based Analysis with R"
    }
    mysubtitle=""
    if("subtitle" %in% data$type) {
        if(shortdata) {
            mysubtitle=data[data$type=="subtitle",]$code[1]
        } else{
            mysubtitle=data[data$type=="subtitle",]$text[1]
        }
        data=data[data$type!="subtitle",]
    }
    if("author" %in% data$type) {
        myauthor=data[data$type=="author",]$code[1]
        data=data[data$type!="author",]
    } else{
        myauthor="prepared by web-r.org"
    }

    if(format=="pptx"){
        mydoc <- read_pptx() %>%
            add_title_slide(title=mytitle,subtitle=ifelse(shortdata,myauthor,mysubtitle))
    } else {
        mydoc <- read_docx()
    }

    for(i in 1:nrow(data)){

        if(showself){
            mydoc=add_self(mydoc,data[i,])
        }

        if(shortdata==0){
            if(class(mydoc)=="rpptx"){
            if(data$type[i]==""){
               if(i<nrow(data)){
                   if(data$type[i+1]!="") next
               } else{
                   next
               }
            }
            }
            echo1=echo|getCodeOption(data$option[i])
            eval=getCodeOption(data$option[i],"eval")
            landscape1=landscape|getCodeOption(data$option[i],"landscape")
            if(data$type[i]=="rcode") {
                echo1=TRUE
                eval=TRUE
            }
             if(data$type[i] %in% c("text","")) {
                 temp=data$text[i]
             } else{
                 temp=""
             }
            if(class(mydoc)=="rpptx"){
                if(data$type[i] %in% c("header2","")){
                    if(!is.na(data$type[i+1])){
                       if(data$type[i+1]=="") temp=data$text[i+1]
                    }
                }
            }
            # temp=data$text[i]
            mydoc=add_text(mydoc,title=data$title[i],text=temp,
                           code=data$code[i],echo=echo1,eval=eval,
                           landscape=landscape1)
        } else{
            echo1=echo
            if(data$type[i]=="rcode") echo1=TRUE

            eval=ifelse(data$type[i]=="text",FALSE,TRUE)
            if(data$type[i] %in% c("mytable","data","plot","table","2plots")) eval=FALSE
            landscape1=FALSE
            if(data$type[i]=="text") {
                temp=data$code[i]
                tempcode=""
            } else {
                temp=""
                tempcode=data$code[i]
            }
            mydoc=add_text(mydoc,title=data$title[i],text=temp,
                           code=tempcode,preprocessing=preprocessing,echo=echo1,eval=eval,
                           landscape=landscape1)
        }


        if(data$type[i]=="rcode") eval(parse(text=data$code[i]))
        if(data$type[i]=="data"){
            # ft=df2flextable2(eval(parse(text=data$code[i])),vanilla=vanilla)

            ft=eval(parse(text=paste0("df2flextable2(",data$code[i],",vanilla=",vanilla,")")))
            mydoc=add_flextable(mydoc,ft,code=data$code[i],echo=echo1,landscape = landscape1)
        } else if(data$type[i]=="ztable"){
            #tempcode=set_argument(data$code[i],argument="vanilla",value=vanilla)
            ft=eval(parse(text=data$code[i]))
            ft<-ztable2flextable(ft)
            mydoc=add_flextable(mydoc,ft,code=data$code[i],echo=echo1,landscape = landscape1)
        } else if(data$type[i]=="flextable"){

            ft=eval(parse(text=data$code[i]))
            mydoc=add_flextable(mydoc,ft,code=data$code[i],echo=echo1,landscape = landscape1)
        } else if(data$type[i]=="table"){
            #tempcode=set_argument(data$code[i],argument="vanilla",value=vanilla)
            ft=eval(parse(text=data$code[i]))
            if("ztable" %in% class(ft)){
                ft<-ztable2flextable(ft)
            }
            mydoc=add_flextable(mydoc,ft,code=data$code[i],echo=echo1,landscape = landscape1)
        } else if(data$type[i]=="mytable"){
            res=eval(parse(text=data$code[i]))
            ft=mytable2flextable(res,vanilla=vanilla)
            mydoc=add_flextable(mydoc,ft,code=data$code[i],echo=echo1,landscape = landscape1)
        } else if(data$type[i]=="ggplot"){
            mydoc=add_ggplot(mydoc,code=data$code[i],preprocessing=preprocessing,top=ifelse(echo1,2,1.5))
        } else if(data$type[i]=="plot"){
            mydoc<-add_plot(mydoc,data$code[i],preprocessing=preprocessing,top=ifelse(echo1,2,1.5))

        } else if(data$type[i] %in% c("2plots","2ggplots")){

            codes=unlist(strsplit(data$code[i],"\n"))
            mydoc=add_2plots(mydoc,plotstring1=codes[1],plotstring2=codes[2],preprocessing=preprocessing,top=ifelse(echo1,2,1.5))

        } else if(data$type[i] %in% c("PNG","png")){

            mydoc<-add_img(mydoc,data$code[i],format="png")

        } else if(data$type[i] %in% c("emf","EMF")){

            mydoc<-add_img(mydoc,data$code[i])

        } else if(str_detect(data$code[i],"df2flextable")){

            tempcode=data$code[i]
            ft=eval(parse(text=tempcode))
            mydoc=add_flextable(mydoc,ft,landscape=landscape1)

        }


    }
    if(length(grep(".",filename,fixed=TRUE))>0) {
        target=filename
    } else{
        target=paste0(filename,".",format)
    }

    mydoc %>% print(target=target)

    # mydoc %>% print(target=".")
    setwd(owd)

    path=str_replace(path,"//","/")
    paste0(path,"/",target)
}

#' convert data to pptx file
#' @param ... arguments to be passed to data2office()
#' @export
#' @examples
#' \donttest{
#' library(rrtable)
#' library(moonBook)
#' library(ggplot2)
#' data2pptx(sampleData2,echo=TRUE)
#' }
data2pptx=function(...){
    data2office(...)
}

#' convert data to docx file
#' @param ... arguments to be passed to data2office()
#' @export
#' @examples
#' \donttest{
#' library(rrtable)
#' library(moonBook)
#' library(ggplot2)
#' data2docx(sampleData2,echo=TRUE)
#' }
data2docx=function(...){
    data2office(...,format="docx")
}


#' Convert html5 code to latex
#' @param df A data.frame
html2latex=function(df){
    temp=colnames(df)
    temp=stringr::str_replace(temp,"<i>p</i>","\\\\textit{p}")
    temp=stringr::str_replace(temp,"&#946;","$\\\\beta$")
    temp=stringr::str_replace(temp,"Beta","$\\\\beta$")
    temp=stringr::str_replace(temp,"&#967;<sup>2</sup>","$\\\\chi^{2}$")
    colnames(df)=temp
    df
}



#'grep string in all files in subdirectory
#'@param x string
#'@param file files to seek
#'@export
mygrep=function(x,file="*"){

    x=substitute(x)
    temp=paste0("grep -r '",x,"' ",file)
    system(temp)
}

# data2docx(sampleData3,echo=TRUE)
