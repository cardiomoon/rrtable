#' Make zipped plots with a data.frame
#' @param data A data.frame
#' @param format Plot format. Choices are c("PNG","SVG","PDF")
#' @param width A plot width
#' @param height A plot height
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi
#' @param start Plot start number
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
myplot2=function(data,format="PNG",width=7,height=7,units="in",res=300,start=0,rawDataName=NULL,rawDataFile="rawData.RDS"){
    filename=c()
    count=nrow(data)

    # cat("in myplot2\n")
    # print(head(data))
    if(!is.null(rawDataName)){
        rawData=readRDS(rawDataFile)
        assign(rawDataName,rawData)
    }


    if(shiny::isRunning()){
        progress <- shiny::Progress$new()
        on.exit(progress$close())
        progress$set(message = "Making Plot", value = 0)
    }

    j=1
    if(count>0) for(i in 1:count){
        #eval(parse(text=data$code[i]))
        # cat("i=",i,"\n")
        if(data$type[i] %in% c("rcode","Rcode")) {
            eval(parse(text=data$code[i]),envir=global_env())
        }

        if(isRunning()){
            progress$inc(1/count, detail = paste("Doing part", i,"/",count))
        }


        if(data$type[i] %in% c("plot","ggplot","PNG","png","emf","code")){

            if(!isRunning()) cat("row:",i,",fig:",j,":",data$code[[i]],"\n")
            path <- paste0("plot_",j,".png")
            filename <- c(filename, path)

            plotPNG2(data$code[i],path,data$type[i],width=width,height=height,units=units,res=res)
            j=j+1

        }
        # else if(!(data$type[i] %in% c("text","title","author"))){
        #         sink("NUL")
        #         eval(parse(text=data$code[i]))
        #         unsink("NUL")
        # }
    }

    filename
}

#' Make png file with a plot code
#' @param x A R code string for plot
#' @param file A path of destination file
#' @param type A character
#' @param width A plot width
#' @param height A plot height
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi
#' @param ggplot A logical. Set this argument true if the R code is for ggplot
#' @importFrom grDevices png
#' @importFrom ggplot2 ggsave
#' @importFrom ggpubr ggexport
plotPNG2=function(x,file,type,width=7,height=7,units="in",res=300,ggplot=FALSE){


    if(is_ggplot(x)) {
        p<-eval(parse(text=x))
        ggsave(file,p,width=width,height=height,units=units,dpi=res)
    } else if(is_ggsurvplot(x)){
        png(file,width=width,height=height,units=units,res=res)
        #pdf(file,paper="letter")
        print(eval(parse(text=x)))
        dev.off()
    } else if(type=="code"){
        p<-eval(parse(text=x))
        if("egg" %in% class(p)){
            ggexport(p,filename=file,width=width*res,height=height*res,res=res)
        }
    } else if(type!="emf"){
        png(file,width=width,height=height,units=units,res=res)
        #pdf(file,paper="letter")
        eval(parse(text=x))
        dev.off()
    } else{
        png(file,width=width,height=height,units=units,res=res)
        #pdf(file,paper="letter")
        print(eval(parse(text=x)))
        dev.off()
    }
}


#' Reports whether plotstring encode a ggsurvplot object
#' @param x A character encoding a plot
#' @export
is_ggsurvplot=function(x){

    p<-eval(parse(text=x))
    ifelse("ggsurvplot" %in% class(p),TRUE,FALSE)
}

#' Make zipped plot file with a data.frame
#' @param data A data.frame
#' @param path A name of destination file path
#' @param filename A path of destination file
#' @param format Plot format. Choices are c("PNG","SVG","PDF")
#' @param width A plot width
#' @param height A plot height
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi
#' @param start Plot start number
#' @param preprocessing A character string of R code
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @param out An object or NULL
#' @importFrom utils zip
#' @export
#' @examples
#' \dontrun{
#' library(moonBook)
#' library(ztable)
#' library(rrtable)
#' library(ggplot2)
#' data2plotzip(sampleData2,path="tmp")
#' }
data2plotzip=function(data,path=".",filename="Plot.zip",format="PNG",width=8,height=6,units="in",res=300,start=0,preprocessing="",
                      rawDataName=NULL,rawDataFile="rawData.RDS",out=NULL){

    obj=ls(envir=global_env())
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
    if(preprocessing!=""){
        #sink("NUL")
        eval(parse(text=preprocessing),envir = global_env())
        #unsink("NUL")
    }
    if(!is.null(out)){
        # str(out)
        for(i in seq_along(out)){
            #assign(names(out)[i],out[[i]])
            # str(out[[i]])
            temp=paste0("assign('",names(out)[i],"',out[[",i,"]],envir=global_env())")
            # cat("temp=",temp,"\n")
            eval(parse(text=temp))
        }
    } else{
        data$type[data$type=="out"]<-"Rcode"
    }

    data=data2to1(data)
    fs=myplot2(data,format=format,width=width,height=height,units=units,res=res,start=start,
               rawDataName=rawDataName,rawDataFile=rawDataFile)
    zip(zipfile=filename, files=fs)
    file.remove(fs)
    setwd(owd)
    if(mode) result=file.copy(paste0(path,"/",filename),filename,overwrite=TRUE)
    path=str_replace(path,"//","/")
    paste0(path,"/",filename)
    objnew=ls(envir=global_env())
    temp=setdiff(objnew,obj)
    rm(list=temp,envir=global_env())

}

data2to1=function(data){
    data1<-data
    data1<-data1[0,]

    for(i in 1:nrow(data)){
        if(data$type[i] %in% c("2ggplots","2plots")){
            if(data$type[i] == "2ggplots"){
                type=c("ggplot","ggplot")
            } else{
                type=c("plot","plot")
            }
            title=c(paste0(data$title[i],"(1/2)"),paste0(data$title[i],"(2/2)"))
            code=unlist(strsplit(data$code[i],"\n"))
            temp=data.frame(type,title,code)
            data1<-rbind(data1,temp)
        } else{
            data1<-rbind(data1,data[i,])
        }

    }
    data1
}


