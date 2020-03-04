#' Save plot/ggplot code to Microsoft Powerpoint format
#' @param ... Further argument to be passed to function dml()
#' @param ggobj a ggplot object
#' @param target name of output file
#' @param append logical value
#' @param title Optional character vector of plot title
#' @param type "pptx" or "docx"
#' @param preprocessing A string of R code or ""
#' @param plottype character  One of c("auto","plot","ggplot","emf")
#' @param echo logical. If true, show code.
#' @param parallel logical. If true, add two plots side by side
#' @param left left margin
#' @param top top margin
#' @param width desired width of the plot
#' @param height desired height of the plot
#' @param aspectr desired aspect ratio of the plot
#' @importFrom stringr "%>%"
#' @importFrom rlang as_label enexprs
#' @importFrom devEMF emf
#' @export
#' @examples
#' \donttest{
#' code2office(plot(iris))
#' require(ggplot2)
#' gg=ggplot(data=mtcars,aes(x=wt,y=mpg))+geom_point()
#' code2office(ggobj=gg)
#' }
code2office=function(...,ggobj=NULL,target="Report",append=FALSE,title="",
                     type="pptx",preprocessing="",plottype="auto",echo=FALSE,parallel=FALSE,
                     left=1,top=1,width=NULL,height=NULL,aspectr=NULL){

    # ggobj=NULL;target="Report";append=FALSE;title=""
    # type="pptx";preprocessing="";plottype="auto";echo=FALSE;parallel=FALSE
    # left=1;top=1;width=NULL;height=NULL;aspectr=NULL

    if(is.null(width)){
        if(is.null(height)){
            if(is.null(aspectr)){
                width=8
                height=5.5
            } else {
                width=8
                height=width/aspectr
            }
        }
    } else {
        if(is.null(height)) {
            if(is.null(aspectr)) {
                height=5.5
            } else{
                height=width/aspectr
            }
        }
    }
    doc<-open_doc(target=target,type=type,append=append)
    target=attr(doc,"name")
    pos=top
            if(title!=""){
                doc <- doc %>% add_text(title=title)
                pos=pos+0.5

            } else {
                if(type=="pptx") doc <- doc %>% add_slide(layout="Title Only")
            }
    if(type=="pptx"){
          if(plottype=="emf"){
              filename="plot.emf"
              temp=unlist(lapply(enexprs(...),rlang::as_label))
              p<-eval(parse(text=temp))
              devEMF::emf(file=filename,width=width,height=height)
              if(is.null(p)) {
                  eval(parse(text=unlist(lapply(enexprs(...),rlang::as_label))))
              } else {
                  print(p)
              }
              dev.off()
              doc<-ph_with(doc,external_img(src="plot.emf",width=width,height=height),
                           location = ph_location(left=left,top=top,
                                                              width=width,height=height))
          } else{
            if(!is.null(ggobj)){
                anyplot=dml(ggobj=ggobj)
            } else{
               # anyplot=eval(parse(text=paste0("dml(code=",rlang::as_label(enquo(code)),")")))
               anyplot=dml(...)
            }
            doc<-ph_with(doc,anyplot,location = ph_location(left=left,top=top,
                                                            width=width,height=height))
          }
    } else{
        if(!is.null(ggobj)){
            doc <- doc %>%
                body_add_gg(value=ggobj)

        } else {
            filename <- tempfile(fileext = ".emf")
            emf(file = filename, width = width, height = height)

            eval(parse(text=unlist(lapply(enexprs(...),rlang::as_label))))
            dev.off()

            doc <- doc %>%
                body_add_img(src = filename, width = width, height = height)
        }
    }


    message(paste0("Exported plot as ", target))
    doc %>% print(target=target)
}

#' Save plot/ggplot code to Microsoft Powerpoint format
#' @param ... further arguments to be passed to code2office
#' @export
#' @examples
#' \donttest{
#' code2pptx(plot(iris))
#' require(ggplot2)
#' gg=ggplot(data=mtcars,aes(x=wt,y=mpg))+geom_point()
#' code2pptx(ggobj=gg)
#' }
code2pptx=function(...){
    code2office(...,type="pptx")
}


#' Save plot/ggplot code to Microsoft Powerpoint format
#' @param ... further arguments to be passed to code2office
#' @export
#' @examples
#' \donttest{
#' code2docx(plot(iris))
#' require(ggplot2)
#' gg=ggplot(data=mtcars,aes(x=wt,y=mpg))+geom_point()
#' code2docx(ggobj=gg)
#' }
code2docx=function(...){
    code2office(...,type="docx")
}



