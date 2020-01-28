#' Save plot/ggplot to Microsoft Powerpoint format
#' @param p An object of class ggplot2 or NULL
#' @param plotstring A character
#' @param target name of output file
#' @param append logical value
#' @param title Optional character of plot title
#' @param type "pptx" or "docx"
#' @param left left margin
#' @param top top margin
#' @param width desired width of the plot
#' @param height desired height of the plot
#' @param aspectr desired aspect ratio of the plot
#' @importFrom stringr "%>%"
#' @export
plot2office=function(p=NULL,plotstring=NULL,target="plot",append=FALSE,title="",
                     type="pptx",
                     left=1,top=1,width=NULL,height=NULL,aspectr=NULL){

   if(type=="pptx"){
      if(!str_detect(target,"\\.")) target=paste0(target,".pptx")
      if(append) doc<-read_pptx(path=target)
      else doc<-read_pptx()
   } else{
      if(!str_detect(target,"\\.")) target=paste0(target,".docx")
      if(append) doc<-read_docx(path=target)
      else doc<-read_docx()
   }
   if(title!=""){
      doc <- doc %>% add_text(title=title)
      top=top+0.5
   } else {
      if(type=="pptx") doc <- doc %>% add_slide(layout="Blank")
   }
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
   if(type=="pptx"){
      if(!is.null(p)){
         doc <- doc %>%
            ph_with(dml(code = print(p)), location = ph_location(left=left,top=top,width=width,height=height))
      } else{
         if(is_ggplot(plotstring)){
            gg=eval(parse(text=plotstring))
            doc <- doc %>%
               ph_with(dml(code = print(gg)), location = ph_location(left=left,top=top,width=width,height=height))
         } else{
            temp=paste0("ph_with(doc,dml(code=",plotstring,"), location = ph_location(left=",left,",top=",top,
                        ",width=",width,",height=",height,"))")
            doc=eval(parse(text=temp))

         }
      }
   } else{
      if(!is.null(p)){
         doc <- doc %>%
            body_add_gg(value=p)

      } else{
         if(is_ggplot(plotstring)){
            p=eval(parse(text=plotstring))
            doc <- doc %>%
               body_add_gg(value=p)
         } else{

            filename <- tempfile(fileext = ".emf")
            emf(file = filename, width = width, height = height)
            eval(parse(text=plotstring))
            dev.off()

            doc <- doc %>%
               body_add_img(src = filename, width = width, height = height)
         }
      }
   }
   message(paste0("Exported plot as ", target))
   doc %>% print(target=target)
}

#' Save plot/ggplot to Microsoft Powerpoint format
#' @param ... further arguments to be passed to plot2office
#' @export
#' @examples
#' require(ggplot2)
#' p<-ggplot(iris,aes(x=Sepal.Length))+geom_histogram()
#' plot2pptx(p)
#' plot2pptx(p,title="A ggplot",append=TRUE)
#' p2=ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point()
#' plot2pptx(p2,append=TRUE)
#' plot2pptx(plotstring="plot(iris)",append=TRUE,title="plot(iris)")
#' plot2pptx(plotstring="ggplot(iris,aes(x=Sepal.Length))+geom_histogram()",append=TRUE)
plot2pptx=function(...){
   plot2office(...,type="pptx")
}

#' Save plot/ggplot to Microsoft Word format
#' @param ... further arguments to be passed to plot2office
#' @export
#' @examples
#' require(ggplot2)
#' p<-ggplot(iris,aes(x=Sepal.Length))+geom_histogram()
#' plot2docx(p)
#' plot2docx(p,title="A ggplot",append=TRUE)
#' p2=ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point()
#' plot2docx(p2,append=TRUE)
#' plot2docx(plotstring="plot(iris)",append=TRUE,title="plot(iris)")
#' plot2docx(plotstring="ggplot(iris,aes(x=Sepal.Length))+geom_histogram()",append=TRUE)
plot2docx=function(...){
   plot2office(...,type="docx")
}

#'Reports whether plotstring encode a ggplot object
#'@param plotstring A character
#'@importFrom ggplot2 is.ggplot
#'@export
#'@examples
#'require(ggplot2)
#'is_ggplot("plot(iris)")
#'is_ggplot("ggplot(iris,aes(x=Sepal.Length))+geom_histogram()")
is_ggplot=function(plotstring){
   x<-eval(parse(text=plotstring))
   ggplot2::is.ggplot(x)
}
