#' Save plot/ggplot to Microsoft Powerpoint format
#' @param p An object of class ggplot2 or NULL
#' @param plotstring A character
#' @param target name of output file
#' @param append logical value
#' @param title Optional character of plot title
#' @importFrom stringr "%>%"
#' @export
#' @examples
#' require(ggplot2)
#' p<-ggplot(iris,aes(x=Sepal.Length))+geom_histogram()
#' plot2pptx(p)
#' plot2pptx(p,title="A ggplot",append=TRUE)
#' p2=ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point()
#' plot2pptx(p2,append=TRUE)
#' plot2pptx(plotstring="plot(iris)")
#' plot2pptx(plotstring="ggplot(iris,aes(x=Sepal.Length))+geom_histogram()",append=TRUE)
plot2pptx=function(p=NULL,plotstring=NULL,target="plot.pptx",append=FALSE,title=""){
   if(append) doc<-read_pptx(path=target)
   else doc<-read_pptx()
   top=1
   if(title!=""){
      doc <- doc %>% add_text(title=title)
      top=1.5
   } else {
      doc <- doc %>% add_slide(layout="Blank")
   }
   if(!is.null(p)){
      doc <- doc %>%
      ph_with(dml(code = print(p)), location = ph_location(left=1,top=top,width=8,height=5.5))
   } else{
      if(is_ggplot(plotstring)){
            gg=eval(parse(text=plotstring))
            doc <- doc %>%
               ph_with(dml(code = print(gg)), location = ph_location(left=1,top=top,width=8,height=5.5))
      } else{
         temp=paste0("ph_with(doc,dml(code=",plotstring,"), location = ph_location(left=1,top=",top,",width=8,height=5.5))")
         doc=eval(parse(text=temp))

      }
   }
   doc %>% print(target=target)
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
