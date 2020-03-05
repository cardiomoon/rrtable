#' read data file and make a pdf file
#' @param file The name of the file which the data are to be read from.
#' @param selected A numeric vector or NULL(default). If specified, only selected data are printed.
#' @param ... Further argument to be passed to data2pdf()
#' @export
file2pdf=function(file,selected=NULL,...){
    data=readCSVComment(file)
    preprocessing=readComment(file)

    if(!is.null(selected)){
         count=nrow(data)
         accept=which((selected>0) & (selected<=count))
         data<-data[selected[accept],]
    }
    data2pdf(data=data,preprocessing=preprocessing,...)
}

#' read data file and make a HTML file
#' @param file The name of the file which the data are to be read from.
#' @param selected A numeric vector or NULL(default). If specified, only selected data are printed.
#' @param ... Further argument to be passed to data2HTML()
#' @export
file2HTML=function(file,selected=NULL,...){
    data=readCSVComment(file)
    preprocessing=readComment(file)

    if(!is.null(selected)){
        count=nrow(data)
        accept=which((selected>0) & (selected<=count))
        data<-data[selected[accept],]
    }
    data2HTML(data=data,preprocessing=preprocessing,...)
}

#' read data file and make a pptx file
#' @param file The name of the file which the data are to be read from.
#' @param selected A numeric vector or NULL(default). If specified, only selected data are printed.
#' @param ... Further argument to be passed to data2pptx()
#' @export
file2pptx=function(file,selected=NULL,...){
    data=readCSVComment(file)
    preprocessing=readComment(file)

    if(!is.null(selected)){
        count=nrow(data)
        accept=which((selected>0) & (selected<=count))
        data<-data[selected[accept],]
    }
    data2pptx(data=data,preprocessing=preprocessing,...)
}

#' read data file and make a pptx file with Rmd file
#' @param file The name of the file which the data are to be read from.
#' @param selected A numeric vector or NULL(default). If specified, only selected data are printed.
#' @param ... Further argument to be passed to data2pptx()
#' @export
file2pptx2=function(file,selected=NULL,...){
    data=readCSVComment(file)
    preprocessing=readComment(file)

    if(!is.null(selected)){
        count=nrow(data)
        accept=which((selected>0) & (selected<=count))
        data<-data[selected[accept],]
    }
    data2pptx2(data=data,preprocessing=preprocessing,...)
}

#' read data file and make a docx file
#' @param file The name of the file which the data are to be read from.
#' @param selected A numeric vector or NULL(default). If specified, only selected data are printed.
#' @param ... Further argument to be passed to data2docx()
#' @export
file2docx=function(file,selected=NULL,...){
    data=readCSVComment(file)
    preprocessing=readComment(file)

    if(!is.null(selected)){
        count=nrow(data)
        accept=which((selected>0) & (selected<=count))
        data<-data[selected[accept],]
    }
    data2docx(data=data,preprocessing=preprocessing,...)
}

#' read data file and make a docx file with Rmd file
#' @param file The name of the file which the data are to be read from.
#' @param selected A numeric vector or NULL(default). If specified, only selected data are printed.
#' @param ... Further argument to be passed to data2docx()
#' @export
file2docx2=function(file,selected=NULL,...){
    data=readCSVComment(file)
    preprocessing=readComment(file)

    if(!is.null(selected)){
        count=nrow(data)
        accept=which((selected>0) & (selected<=count))
        data<-data[selected[accept],]
    }
    data2docx2(data=data,preprocessing=preprocessing,...)
}

#' read data file and make a zip file with plots
#' @param file The name of the file which the data are to be read from.
#' @param selected A numeric vector or NULL(default). If specified, only selected data are printed.
#' @param ... Further argument to be passed to data2plotzip()
#' @export
file2plotzip=function(file,selected=NULL,...){
    data=readCSVComment(file)
    preprocessing=readComment(file)

    if(!is.null(selected)){
        count=nrow(data)
        accept=which((selected>0) & (selected<=count))
        data<-data[selected[accept],]
    }
    data2plotzip(data=data,preprocessing=preprocessing,...)
}
