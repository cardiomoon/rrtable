#' Save plot/ggplot to Microsoft Powerpoint format
#' @param x An object of class ggplot2 or a string vector encoding plot or ggplot
#' @param target name of output file
#' @param append logical value
#' @param title Optional character vector of plot title
#' @param type "pptx" or "docx"
#' @param preprocessing A string of R code or ""
#' @param echo logical. If true, show code.
#' @param parallel logical. If true, add two plots side by side
#' @param left left margin
#' @param top top margin
#' @param width desired width of the plot
#' @param height desired height of the plot
#' @param aspectr desired aspect ratio of the plot
#' @importFrom stringr "%>%"
#' @export
#' @examples
#' \donttest{
#' x=c("plot(iris)","ggplot(mtcars,aes(x=hp,y=mpg))+geom_point()")
#' plot2office(x,title="2 plots",parallel=TRUE)
#' plot2office(x,title="2 plots",parallel=TRUE,echo=TRUE,append=TRUE)
#' plot2office(x,parallel=TRUE,echo=TRUE,append=TRUE)
#' }
plot2office=function(x=NULL,target="Report",append=FALSE,title="",
                     type="pptx",preprocessing="",echo=FALSE,parallel=FALSE,
                     left=1,top=1,width=NULL,height=NULL,aspectr=NULL){
   if(is.null(x)) {
       message("x should be a ggplot object or a string encoding plot or ggplot")
       return()
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
   doc<-open_doc(target=target,type=type,append=append)
   target=attr(doc,"name")
   if(is.character(x)) {
      count=length(x)
   } else{
      count=1
   }
   if((count==2)&parallel){
      pos=top
      if(title[1]!=""){
         doc <- doc %>% add_text(title=title)
         pos=pos+0.5
      } else {
         if(type=="pptx") doc <- doc %>% add_slide(layout="Blank")
      }
      if(echo & is.character(x)) {
         codes=stringr::str_c(x,collapse="\n")
         codeft=Rcode2flextable(codes,preprocessing=preprocessing,format="pptx",eval=FALSE)
         doc<-doc %>% ph_with(value=codeft, location = ph_location(left=1,top=pos))
         pos=pos+0.8
      }
      doc <- doc %>% add_2plots(plotstring1=x[1],plotstring2=x[2],top=pos)

   } else{
   for(i in 1:count){
   pos=top
   if((length(title)>=i)&(title[i]!="")){
      doc <- doc %>% add_text(title=title[i])
      pos=pos+0.5

   } else {
      if(type=="pptx") doc <- doc %>% add_slide(layout="Blank")
   }
   if(is.character(x)){code=x[i]}
   else code=x

      if(echo & is.character(code)) {

         codeft=Rcode2flextable(code,preprocessing=preprocessing,format="pptx")
         doc<-doc %>% ph_with(value=codeft, location = ph_location(left=1,top=pos))
         pos=pos+0.5

      }
      doc<- add_anyplot(doc,x=code,preprocessing=preprocessing,left=left,top=pos,width=width,height=height)
   }


   }
   message(paste0("Exported plot as ", target))
   doc %>% print(target=target)
}

#' Save plot/ggplot to Microsoft Powerpoint format
#' @param ... further arguments to be passed to plot2office
#' @export
#' @examples
#' \donttest{
#' require(ggplot2)
#' x<-ggplot(iris,aes(x=Sepal.Length))+geom_histogram()
#' plot2pptx(x)
#' plot2pptx(x,title="A ggplot",append=TRUE)
#' p2=ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point()
#' plot2pptx(p2,append=TRUE)
#' plot2pptx(x=c("plot(iris)","ggplot(iris,aes(x=Sepal.Length))+geom_histogram()"),
#'     append=TRUE,title=c("plot","ggplot"),echo=TRUE)
#' }
plot2pptx=function(...){
   plot2office(...,type="pptx")
}

#' Save plot/ggplot to Microsoft Word format
#' @param ... further arguments to be passed to plot2office
#' @export
#' @examples
#' \donttest{
#' require(ggplot2)
#' x<-ggplot(iris,aes(x=Sepal.Length))+geom_histogram()
#' plot2docx(x)
#' plot2docx(x,title="A ggplot",append=TRUE)
#' p2=ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point()
#' plot2docx(p2,append=TRUE)
#' plot2docx(x="plot(iris)",append=TRUE,title="plot(iris)")
#' plot2docx(x="ggplot(iris,aes(x=Sepal.Length))+geom_histogram()",append=TRUE)
#' }
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


#' Make/open office document with file name
#' @param target name of output file
#' @param type "pptx" or "docx"
#' @param append logical
#' @export
open_doc=function(target="Report", type="pptx",append=FALSE) {
   if(type=="pptx"){
      if(!str_detect(target,"\\.")) target=paste0(target,".pptx")
      if(append & file.exists(target)) doc<-read_pptx(path=target)
      else doc<-read_pptx()
   } else{
      if(!str_detect(target,"\\.")) target=paste0(target,".docx")
      if(append & file.exists(target)) doc<-read_docx(path=target)
      else doc<-read_docx()
   }
   attr(doc,"name")=target
   doc
}


#' Add a ggplot or a plot to the Microsoft Office Document
#' @param doc A document object
#' @param x An object of class ggplot2 or a string encoding plot or ggplot
#' @param preprocessing A string of R code
#' @param left left margin
#' @param top top margin
#' @param width desired width of the plot
#' @param height desired height of the plot
#' @export
add_anyplot=function(doc,x=NULL,preprocessing="",left=1,top=1,width=8,height=5.5){

   if(preprocessing!="") {
      eval(parse(text=preprocessing))
   }
   if(class(doc)=="rpptx"){
      if(is.ggplot(x)){
         doc <- doc %>%
            ph_with(dml(code = print(x)), location = ph_location(left=left,top=top,width=width,height=height))
      } else{
         if(is_ggplot(x)){
            gg=eval(parse(text=x))
            doc <- doc %>%
               ph_with(dml(code = print(gg)), location = ph_location(left=left,top=top,width=width,height=height))
         } else{
            temp=paste0("ph_with(doc,dml(code=",x,"), location = ph_location(left=",left,",top=",top,
                        ",width=",width,",height=",height,"))")
            doc=eval(parse(text=temp))

         }
      }
   } else{
      if(is.ggplot(x)){
         doc <- doc %>%
            body_add_gg(value=x)

      } else{
         if(is_ggplot(x)){
            gg=eval(parse(text=x))
            doc <- doc %>%
               body_add_gg(value=gg,width=width,height=height)
         } else{

            filename <- tempfile(fileext = ".emf")
            emf(file = filename, width = width, height = height)
            eval(parse(text=x))
            dev.off()

            doc <- doc %>%
               body_add_img(src = filename, width = width, height = height)
         }
      }
   }
   doc
}
