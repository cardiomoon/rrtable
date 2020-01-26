#' Add plot into a document object
#' @param mydoc A document object
#' @param plotstring An string of R code encoding plot
#' @param width the width of the device.
#' @param height the height of the device.
#' @param units The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.
#' @param res The nominal resolution in ppi which will be recorded in the bitmap file, if a positive integer. Also used for units other than the default, and to convert points to pixels.
#' @param format plot format
#' @param ... additional arguments passed to png()
#' @return a document object
#' @importFrom devEMF emf
#' @importFrom officer body_add_img
#' @export
#' @examples
#' require(officer)
#' require(rrtable)
#' require(magrittr)
#' require(flextable)
#' read_pptx() %>% add_text(title="Add image") %>% add_img("plot(mtcars)",format="png",res=300)
add_img=function(mydoc,plotstring,width=7,height=5,units="in",
                 res=300,format="emf",...) {
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

            temp=paste0("ph_with(mydoc,value = external_img(src=filename), use_loc_size = TRUE, location = ph_location(left=1,top=2,width=8,height=5))")
            mydoc=eval(parse(text=temp))

    } else{

        mydoc <- body_add_img(mydoc,src=filename,
                              width=width,height=height)
    }
    mydoc
}
