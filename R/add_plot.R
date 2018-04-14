#' Add plot into a document object
#' @param mydoc A document object
#' @param plotstring String of an R code encoding a plot
#' @param top top position of plot
#' @return a document object
#' @importFrom officer ph_with_text
#' @importFrom rvg ph_with_vg_at
#' @export
#' @examples
#' require(rrtable)
#' require(officer)
#' require(rvg)
#' require(magrittr)
#' read_pptx() %>% add_text(title="Plot") %>% add_plot("plot(iris)")
add_plot=function(mydoc,plotstring,top=2){

    if(class(mydoc)=="rpptx"){
        temp=paste0("ph_with_vg_at(mydoc,code=",plotstring,",left=1,top=",top,",width=8,height=5)")
        mydoc=eval(parse(text=temp))

    } else{
        temp=paste0("body_add_vg(mydoc,code=",plotstring,")")
        mydoc=eval(parse(text=temp))
    }
    mydoc
}
