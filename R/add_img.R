#' Add plot into a document object
#' @param mydoc A document object
#' @param plotstring An string of R code encoding plot
#' @param title An character string as a plot title
#' @param width the width of the device.
#' @param height the height of the device.
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer. Also used for units other than the default, and to convert points to pixels.
#' @param format plot format
#' @param echo logical Whether or not show R code
#' @param ... additional arguments passed to png()
#' @return a document object
#' @importFrom devEMF emf
#' @importFrom officer ph_with_img body_add_img ph_with_img_at
#' @export
#' @examples
#' require(officer)
#' require(rrtable)
#' require(magrittr)
#' require(flextable)
#' read_pptx() %>% add_img("plot(mtcars)",format="png",res=300)
add_img=function(mydoc,plotstring,title="",width=7,height=5,units="in",
                 res=300,format="emf",echo=FALSE,...) {
    # produce an emf file containing the ggplot
    filename <- tempfile(fileext = paste0(".",format))
    if(format=="emf"){
        emf(file = filename, width = width, height = height)
    } else if(format %in% c("png","PNG")){
        png(filename = filename, width = width, height = height,units=units,res=res,...)
    }
    eval(parse(text=plotstring))
    dev.off()
    if(class(mydoc)=="rpptx"){
        mydoc<- mydoc %>%
            add_slide(layout = "Title and Content", master = "Office Theme") %>%
            ph_with_text(type="title",str=title)
        if(echo){
            codeft=Rcode2flextable(plotstring,eval=FALSE,format="pptx")
            mydoc<-mydoc %>% ph_with_flextable_at(value=codeft,left=1,top=2)
            temp=paste0("ph_with_img_at(mydoc,src=filename,left=1,top=2.3,width=8,height=5)")
            mydoc=eval(parse(text=temp))
        } else{
            temp=paste0("ph_with_img_at(mydoc,src=filename,left=1,top=2,width=8,height=5)")
            mydoc=eval(parse(text=temp))
        }
    } else{
        mydoc <- mydoc %>% add_title(title)
        if(echo){
            codeft=Rcode2flextable(plotstring,eval=FALSE,format="docx")
            mydoc<-mydoc %>% body_add_flextable(codeft)
        }
        mydoc <- body_add_img(mydoc,src=filename,
                              width=width,height=height)
    }
    mydoc
}

# read_docx() %>% add_img(plot(mtcars),title="plot(mtcars)",format="png",res=300) %>%
#      print(target="png.docx")

