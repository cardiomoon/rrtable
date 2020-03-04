#' Add a ggplot or a plot to the Microsoft Office Document
#' @param doc A document object
#' @param x A string encoding plot or ggplot
#' @param preprocessing A string of R code or ""
#' @param left left margin
#' @param top top margin
#' @param width desired width of the plot
#' @param height desired height of the plot
#' @export
add_emf=function(doc,x,preprocessing="",left=1,top=1,width=8,height=5.5){

    if(preprocessing!="") {
        eval(parse(text=preprocessing))
    }

    filename <- tempfile(fileext = ".emf")
    p<-eval(parse(text=x))
    emf(file = filename, width = 8, height = 5.5)
    if(!is.null(p)) {
        print(p)
    } else{
        eval(parse(text=x))
    }
    dev.off()
    doc<-ph_with(doc,external_img(src = filename, width = width, height = height),
            location = ph_location(left = left, top = top, width = width, height = height) )
    doc
}

#' Save plot/ggplot to Microsoft Powerpoint format
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
#' require(ggplot2)
#' emf2pptx("ggplot(data=iris,aes(x=Sepal.Length))+geom_density()")
emf2pptx=function(x,target="Report",append=FALSE,title="",
                  type="pptx",preprocessing="",
                  left=1,top=1,width=8,height=5.5){

    # read_pptx() %>%
    doc<-open_doc(target=target,type=type,append=append)
    target=attr(doc,"name")
        # add_slide(layout = "Title and Content", master = "Office Theme") %>%
        # ph_with(external_img(src = filename, width = 6, height = 7),
        #         location = ph_location_type(type = "body"), use_loc_size = FALSE ) %>%
        doc<-doc %>%
            add_slide(layout = "Title Only") %>%
            add_emf(x,preprocessing=preprocessing,left = left, top = top, width = width, height = height)
        # ph_with(external_img(src = filename, width = 8, height = 5.5),
        #         location = ph_location(left = 1, top = 1, width = 8, height = 5.5) ) %>%
        message(paste0("Exported plot as ", target))
        print(doc,target=target)
}


