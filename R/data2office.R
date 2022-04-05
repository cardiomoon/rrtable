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
#' @param out An object or NULL
#' @importFrom officer read_docx read_pptx
#' @importFrom ztable ztable2flextable
#' @importFrom shiny isRunning Progress
#' @importFrom rlang global_env
#' @export
data2office=function(data,
                     preprocessing="",
                     path=".",filename="Report",format="pptx",width=7,height=5,units="in",
                     res=300,rawDataName=NULL,rawDataFile="rawData.RDS",vanilla=FALSE,echo=FALSE,
                     landscape=FALSE,
                     showself=FALSE,
                     out=NULL){

     # data=readCSVComment("~/Downloads/PPTxList.csv")
    # preprocessing=""
    # path=".";filename="Report";format="pptx";width=7;height=5;units="in"
    # res=300;rawDataName=NULL;rawDataFile="rawData.RDS";vanilla=FALSE;echo=FALSE
    # landscape=FALSE;
    # showself=FALSE


    datadata=data
    obj=ls(envir=global_env())

    if(preprocessing!=""){
        #sink("NUL")
        eval(parse(text=preprocessing),envir = global_env())
        #unsink("NUL")
    }
    if(!is.null(attr(datadata,"preprocessing"))){
        temp=attr(datadata,"preprocessing")
        eval(parse(text=temp),envir=global_env())
    }

    if(!is.null(out)){
        for(i in seq_along(out)){
            temp=paste0("assign('",names(out)[i],"',out[[i]],envir=global_env())")
            eval(parse(text=temp))
            #assign(names(out)[i],out[[i]])
        }
    }

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


    datadata$type=tolower(datadata$type)

    if(ncol(datadata)==3) {
        shortdata=1
    } else {
        shortdata=0
    }

    if("title" %in% datadata$type) {
        if(shortdata) {
            mytitle=datadata[datadata$type=="title",]$code[1]
        } else{
            mytitle=datadata[datadata$type=="title",]$text[1]
        }
        datadata=datadata[datadata$type!="title",]
    } else{
        mytitle="Web-based Analysis with R"
    }
    mysubtitle=""
    if("subtitle" %in% datadata$type) {
        if(shortdata) {
            mysubtitle=datadata[datadata$type=="subtitle",]$code[1]
        } else{
            mysubtitle=datadata[datadata$type=="subtitle",]$text[1]
        }
        datadata=datadata[datadata$type!="subtitle",]
    }
    if("author" %in% datadata$type) {
        myauthor=datadata[datadata$type=="author",]$code[1]
        datadata=datadata[datadata$type!="author",]
    } else{
        myauthor="prepared by web-r.org"
    }

    if(format=="pptx"){
        mydoc <- read_pptx() %>%
            add_title_slide(title=mytitle,subtitle=ifelse(shortdata,myauthor,mysubtitle))
    } else {
        mydoc <- read_docx()
    }

    if(shiny::isRunning()){
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Making File", value = 0)
    } else{
        cat("Making File: 1")
    }

    for(i in 1:nrow(datadata)){


        if(isRunning()){
            progress$inc(1/(nrow(datadata)), detail = paste("Doing part", i+1,"/",nrow(datadata)+1))
        } else(
            cat(i+1)
        )

        if(showself){
            mydoc=add_self(mydoc,datadata[i,])
        }

        if(shortdata==0){
            if(inherits(mydoc,"rpptx")){
            if(datadata$type[i]==""){
               if(i<nrow(datadata)){
                   if(datadata$type[i+1]!="") next
               } else{
                   next
               }
            }
            }
            echo1=echo|getCodeOption(datadata$option[i])
            eval=getCodeOption(datadata$option[i],"eval")
            landscape1=landscape|getCodeOption(datadata$option[i],"landscape")
            if(datadata$type[i] %in% c("rcode","Rcode","Pre","pre")) {
                echo1=TRUE
                eval=TRUE
            }
             if(datadata$type[i] %in% c("text","")) {
                 temp=datadata$text[i]
             } else{
                 temp=""
             }
            if(inherits(mydoc,"rpptx")){
                if(datadata$type[i] %in% c("header2","")){
                    if(!is.na(datadata$type[i+1])){
                       if(datadata$type[i+1]=="") temp=datadata$text[i+1]
                    }
                }
            }

            if(inherits(mydoc,"rpptx") & datadata$type[i]=="code"){
                mydoc<-mydoc %>% add_slide(layout="Title Only")
                mydoc<-mydoc %>%
                    ph_with(value=datadata$title[i],location=ph_location_type(type="title"))
            } else if(datadata$type[i]!="eval"){
            mydoc=add_text(mydoc,title=datadata$title[i],text=temp,
                           code=datadata$code[i],echo=echo1,eval=eval,
                           landscape=landscape1)
            }
        } else{
            echo1=echo
            if(datadata$type[i] %in% c("rcode","Rcode","html","HTML","Pre","pre")) {
                echo1=TRUE
            } else if(datadata$type[i] %in% c("mytable","data","plot","table","2plots")) {
                eval=FALSE
            } else if(datadata$type[i]=="eval"){
                echo1=FALSE
                eval=TRUE
            }
            eval=ifelse(datadata$type[i]=="text",FALSE,TRUE)
            landscape1=FALSE
            if(datadata$type[i] %in% c("text","out")) {
                temp=datadata$code[i]
                tempcode=""
            } else {
                temp=""
                tempcode=datadata$code[i]
            }
            if(inherits(mydoc,"rpptx") & datadata$type[i]=="code"){
                mydoc<-mydoc %>% add_slide(layout="Title Only")
                mydoc<-mydoc %>%
                    ph_with(value=datadata$title[i],location=ph_location_type(type="title"))
            } else if(datadata$type[i]!="eval"){
            mydoc=add_text(mydoc,title=datadata$title[i],text=temp,
                           code=tempcode,echo=echo1,eval=eval,
                           landscape=landscape1)
            }
        }

        if(datadata$type[i] %in% c("out","Out")) {
            if(is.null(out)) {
                eval(parse(text=datadata$code[i]),envir=global_env())
            }
        } else if(datadata$type[i] %in% c("rcode","Rcode","html","HTML","Pre","pre","eval")) {
            #sink("NUL")
            eval(parse(text=datadata$code[i]),envir=global_env())
            #unsink("NUL")
            # preprocessing=paste0(preprocessing,"\n",datadata$code[i])
        } else if(datadata$type[i]=="data"){
            # ft=df2flextable2(eval(parse(text=data$code[i])),vanilla=vanilla)

            ft=eval(parse(text=paste0("df2flextable2(",datadata$code[i],",vanilla=",vanilla,")")))
            mydoc=add_flextable(mydoc,ft,code=datadata$code[i],echo=echo1,landscape = landscape1)
        } else if(datadata$type[i]=="data2"){
            ft=eval(parse(text=paste0("myFlextable(",datadata$code[i],")")))
            mydoc=add_flextable(mydoc,ft,code=datadata$code[i],echo=echo1,landscape = landscape1)
        }else if(datadata$type[i]=="ztable"){
            #tempcode=set_argument(datadata$code[i],argument="vanilla",value=vanilla)
            ft=eval(parse(text=datadata$code[i]))
            ft<-ztable2flextable(ft)
            mydoc=add_flextable(mydoc,ft,code=datadata$code[i],echo=echo1,landscape = landscape1)
        } else if(datadata$type[i]=="flextable"){

            ft=eval(parse(text=datadata$code[i]))
            mydoc=add_flextable(mydoc,ft,code=datadata$code[i],echo=echo1,landscape = landscape1)
        } else if(datadata$type[i]=="table"){
            #tempcode=set_argument(datadata$code[i],argument="vanilla",value=vanilla)
            ft=eval(parse(text=datadata$code[i]))
            if("ztable" %in% class(ft)){
                ft<-ztable2flextable(ft)
            }
            mydoc=add_flextable(mydoc,ft,code=datadata$code[i],echo=echo1,landscape = landscape1)
        } else if(datadata$type[i]=="mytable"){
            res=eval(parse(text=datadata$code[i]))
            ft=mytable2flextable(res,vanilla=vanilla)
            mydoc=add_flextable(mydoc,ft,code=datadata$code[i],echo=echo1,landscape = landscape1)
        } else if(datadata$type[i] %in% c("ggplot","plot","girafe")){
            if(is.null(out)){
            mydoc=add_anyplot(mydoc,x=datadata$code[i],top=ifelse(echo1,2,1.5))
            } else{
            mydoc=add_anyplot(mydoc,x=datadata$code[i],top=ifelse(echo1,2,1.5))
            }
        } else if(datadata$type[i] %in% c("2plots","2ggplots")){

            codes=unlist(strsplit(datadata$code[i],"\n"))
            mydoc=add_2plots(mydoc,plotstring1=codes[1],plotstring2=codes[2],top=ifelse(echo1,2,1.5))

        } else if(datadata$type[i] %in% c("PNG","png")){

            mydoc<-add_image(mydoc,datadata$code[i],format="png")

        } else if(datadata$type[i] %in% c("emf","EMF")){

            mydoc<-add_image(mydoc,datadata$code[i])

        } else if(str_detect(datadata$code[i],"df2flextable")){

            tempcode=datadata$code[i]
            ft=eval(parse(text=tempcode))
            mydoc=add_flextable(mydoc,ft,landscape=landscape1)

        } else if(datadata$type[i]=="code"){
            filename1="plot.emf"
            devEMF::emf(file=filename1,width=8,height=5.5)
            suppressWarnings(eval(parse(text=datadata$code[i])))
            dev.off()
            mydoc<-ph_with(mydoc,external_img(src="plot.emf",width=8,height=5.5),
                         location = ph_location(left=1,top=1.5,
                                                width=8,height=5.5))
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
    if(!isRunning()) cat("\n")
    objnew=ls(envir=global_env())
    temp=setdiff(objnew,obj)
    rm(list=temp,envir=global_env())
    path=str_replace(path,"//","/")
    paste0(path,"/",target)
}

#' convert data to pptx file
#' @param ... arguments to be passed to data2office()
#' @export
#' @examples
#' \dontrun{
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
#' \dontrun{
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


