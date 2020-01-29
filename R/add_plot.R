#' Add plot into a document object
#' @param mydoc A document object
#' @param plotstring String of an R code encoding a plot
#' @param preprocessing preprocessing
#' @param width width of plot
#' @param height height of plot
#' @param top top position of plot
#' @return a document object
#' @export
#' @examples
#' require(rrtable)
#' require(officer)
#' require(rvg)
#' require(magrittr)
#' read_pptx() %>% add_text(title="Plot") %>% add_plot("plot(iris)")
#' read_docx() %>% add_text(title="Plot") %>% add_plot("plot(iris)")
add_plot=function(mydoc,plotstring,preprocessing="",width=6,height=6,top=2){

    if(preprocessing!="") {
        eval(parse(text=preprocessing))
    }

    if(class(mydoc)=="rpptx"){
        temp=paste0("ph_with(mydoc,dml(code=",plotstring,"), location = ph_location(left=1,top=",top,",width=8,height=5))")
        mydoc=eval(parse(text=temp))

    } else{
        filename <- tempfile(fileext = ".emf")
        emf(file = filename, width = width, height = height)
        eval(parse(text=plotstring))
        dev.off()

        mydoc <- mydoc %>%
            body_add_img(src = filename, width = width, height = height)
    }
    mydoc
}
