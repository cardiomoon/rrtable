#' Make zipped plots with a data.frame
#' @param data A data.frame
#' @param format Plot format. Choices are c("PNG","SVG","PDF")
#' @param width A plot width
#' @param height A plot height
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi
#' @param start Plot start number
#' @param preprocessing A character string of R code
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
myplot2=function(data,format="PNG",width=7,height=7,units="in",res=300,start=0,preprocessing="",rawDataName=NULL,rawDataFile="rawData.RDS"){
    filename=c()
    count=nrow(data)


    if(!is.null(rawDataName)){
        rawData=readRDS(rawDataFile)
        assign(rawDataName,rawData)
    }

    if(preprocessing!=""){
        eval(parse(text=preprocessing))
    }
    j=1
    if(count>0) for(i in 1:count){
        #eval(parse(text=data$code[i]))
        if(data$type[i] %in% c("plot","ggplot","PNG","png")){
            path <- paste("plot_", j+start, ".",format, sep="")
            filename <- c(filename, path)
            if(format=="SVG"){
                temp=paste0("plot",format,"2(function(){",data$code[[i]],"},path,width=width,height=height")
                if(data$type[i]=="ggplot"){
                    temp=paste0(temp,",ggplot=TRUE")
                }
                temp=paste0(temp,")")
            } else{
                temp=paste0("plot",format,"2(function(){",data$code[[i]],"},path,width=width,height=height,units=units,res=res")
                if(data$type[i]=="ggplot"){
                    temp=paste0(temp,",ggplot=TRUE")
                }
                temp=paste0(temp,")")
            }
            eval(parse(text=temp))
            j=j+1
        } else if(!(data$type[i] %in% c("text","title","author"))){
            eval(parse(text=data$code[i]))
        }
    }
    filename
}

#' Make pdf file with a plot code
#' @param fun A R code for plot
#' @param file A path of destination file
#' @param width A plot width
#' @param height A plot height
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi
#' @param ggplot A logical. Set this argument true if the R code is for ggplot
#' @importFrom grDevices cairo_pdf dev.off
plotPDF2=function(fun,file,width=7,height=5,units="in",res=300,ggplot=FALSE){

    if(ggplot){
        fun()
        ggsave(file,width=width,device=cairo_pdf,height=height,units=units,dpi=res)
    }
    else {
        cairo_pdf(file,width=width,height=height)
        #pdf(file,paper="letter")
        fun()
        dev.off()
    }

}

#' Make SVG file with a plot code
#' @param fun A R code for plot
#' @param file A path of destination file
#' @param width A plot width
#' @param height A plot height
#' @param ggplot A logical. Set this argument true if the R code is for ggplot
#' @importFrom grDevices svg
plotSVG2=function(fun,file,width=7,height=7,ggplot=FALSE){

    if(ggplot) ggsave(file,fun(),width=width,height=height)
    else {
        svg(file,width=width,height=height)
        #pdf(file,paper="letter")
        fun()
        dev.off()
    }

}

#' Make png file with a plot code
#' @param fun A R code for plot
#' @param file A path of destination file
#' @param width A plot width
#' @param height A plot height
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi
#' @param ggplot A logical. Set this argument true if the R code is for ggplot
#' @importFrom grDevices png
#' @importFrom ggplot2 ggsave
plotPNG2=function(fun,file,width=7,height=7,units="in",res=300,ggplot=FALSE){

    if(ggplot) ggsave(file,fun(),width=width,height=height,units=units,dpi=res)
    else {
        png(file,width=width,height=height,units=units,res=res)
        #pdf(file,paper="letter")
        fun()
        dev.off()
    }

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
#' @importFrom utils zip
#' @export
#' @examples
#' library(moonBook)
#' library(ztable)
#' library(rrtable)
#' library(ggplot2)
#' data2plotzip(sampleData2,path="tmp")
data2plotzip=function(data,path=NULL,filename="Plot.zip",format="PNG",width=8,height=6,units="in",res=300,start=0,preprocessing="",
                      rawDataName=NULL,rawDataFile="rawData.RDS"){

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

    data=data2to1(data)
    fs=myplot2(data,format=format,width=width,height=height,units=units,res=res,start=start,preprocessing=preprocessing,
               rawDataName=rawDataName,rawDataFile=rawDataFile)
    zip(zipfile=filename, files=fs)
    setwd(owd)
    if(mode) result=file.copy(paste0(path,"/",filename),filename,overwrite=TRUE)
    path=str_replace(path,"//","/")
    paste0(path,"/",filename)


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


