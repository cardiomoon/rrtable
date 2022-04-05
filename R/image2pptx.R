#' Add plot into a document object
#' @param mydoc A document object
#' @param x An string of R code encoding plot
#' @param preprocessing A string of R code or ""
#' @param left left margin
#' @param top top margin
#' @param width the width of the device.
#' @param height the height of the device.
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer. Also used for units other than the default, and to convert points to pixels.
#' @param format plot format
#' @param ... additional arguments passed to png()
#' @return a document object
#' @importFrom devEMF emf
#' @importFrom officer body_add_img external_img ph_with
#' @export
#' @examples
#' require(officer)
#' require(rrtable)
#' require(magrittr)
#' require(ggplot2)
#' read_pptx() %>% add_text(title="Add image") %>% add_image("plot(iris)")
#' read_docx() %>% add_text(title="Add image") %>% add_image("plot(1:10)",format="png")
add_image=function(mydoc,x=NULL,preprocessing="",left=1,top=2,width=8,height=5.5,units="in",
                 res=300,format="emf",...) {

  if(is.null(x)) {
    message("x should be a ggplot object or a string encoding plot or ggplot")
    return()
  }

     if(preprocessing!="") {
          eval(parse(text=preprocessing))
    }

    if(is.character(x)) {
      p<-eval(parse(text=x))
    } else{
       p<-x
    }

if(format=="emf"){
        filename <- tempfile(fileext = ".emf")
        emf(file = filename, width = width, height = height)
    } else if(format %in% c("png","PNG")){
        filename <- tempfile(fileext = ".png")
        png(filename = filename, width = width, height = height,units=units,res=res)
    }
    if(is.null(p)) {
      eval(parse(text=x))
    } else{
      print(p)
    }
    dev.off()
    if(inherits(mydoc,"rpptx")){

            mydoc<-ph_with(mydoc,value = external_img(src=filename,width=width,height=height),
              use_loc_size = TRUE, location = ph_location(left=left,top=top,width=width,height=height))

    } else{

        mydoc <- body_add_img(mydoc,src=filename,
                              width=width,height=height)
    }
    mydoc
     # mydoc %>% print(target="test.pptx")
}

#' Save plot/ggplot as image to Microsoft Powerpoint format
#' @param x A string vector encoding plot or ggplot
#' @param target name of output file
#' @param append logical value
#' @param title Optional character vector of plot title
#' @param type "pptx" or "docx"
#' @param preprocessing A string of R code or ""
#' @param left left margin
#' @param top top margin
#' @param width desired width of the plot
#' @param height desired height of the plot
#' @importFrom stringr "%>%"
#' @export
#' @examples
#' \dontrun{
#' require(ggplot2)
#' image2pptx("ggplot(data=iris,aes(x=Sepal.Length))+geom_density()")
#' }
image2office=function(x,target="Report",append=FALSE,title="",
                  type="pptx",preprocessing="",
                  left=1,top=1,width=8,height=5.5){

  # read_pptx() %>%
  if(preprocessing!=""){
    #sink("NUL")
    eval(parse(text=preprocessing),envir = global_env())
    #unsink("NUL")
  }
  doc<-open_doc(target=target,type=type,append=append)
  target=attr(doc,"name")
  # add_slide(layout = "Title and Content", master = "Office Theme") %>%
  # ph_with(external_img(src = filename, width = 6, height = 7),
  #         location = ph_location_type(type = "body"), use_loc_size = FALSE ) %>%
  doc<-doc
  if(title!="") {
    doc <- doc %>% add_text(title=title)
  } else{
    if(type=="pptx") doc <- doc %>% add_slide(layout = "Title Only")
  }

  doc <- doc %>%
    add_image(x,left = left, top = top, width = width, height = height)
  # ph_with(external_img(src = filename, width = 8, height = 5.5),
  #         location = ph_location(left = 1, top = 1, width = 8, height = 5.5) ) %>%
  message(paste0("Exported plot as ", target))
  print(doc,target=target)
}


#' Save plot/ggplot to Microsoft Powerpoint format
#' @param ... further arguments to be passed to image2office
#' @export
#' @examples
#' \dontrun{
#' require(ggplot2)
#' x<-ggplot(iris,aes(x=Sepal.Length))+geom_histogram()
#' image2pptx(x)
#' x="plot(iris)"
#' image2pptx(x,title="A plot",append=TRUE)
#' p2="ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point()"
#' image2pptx(p2,append=TRUE)
#' }
image2pptx=function(...){
  image2office(...)
}

#' Save plot/ggplot to Microsoft Word format
#' @param ... further arguments to be passed to image2office
#' @export
#' @examples
#' \dontrun{
#' require(ggplot2)
#' x<-ggplot(iris,aes(x=Sepal.Length))+geom_histogram()
#' image2docx(x)
#' image2docx(x="plot(iris)",title="A ggplot",append=TRUE)
#' p2="ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point()"
#' image2docx(p2,append=TRUE)
#' }
image2docx=function(...){
  image2office(...,type="docx")
}
