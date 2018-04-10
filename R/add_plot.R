#' Add plot into a document object
#' @param mydoc A document object
#' @param plotstring String of an R code encoding a plot
#' @param title An character string as a plot title
#' @param vector A logical. If TRUE, vector graphics are produced instead, PNG images if FALSE.
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
add_plot=function(mydoc,plotstring,title="",vector=TRUE){

    if(class(mydoc)=="rpptx"){

        # mydoc<- mydoc %>%
        #     add_slide(layout = "Title and Content", master = "Office Theme") %>%
        #     ph_with_text(type="title",str=title)
        # if(echo){
        #     codeft=Rcode2flextable(plotstring,eval=FALSE,format="pptx")
        #     mydoc<-mydoc %>% ph_with_flextable_at(value=codeft,left=1,top=2)
        #     temp=paste0("ph_with_vg_at(mydoc,code=",plotstring,",left=1,top=2.3,width=8,height=5)")
        #     mydoc=eval(parse(text=temp))
        # } else{
            temp=paste0("ph_with_vg_at(mydoc,code=",plotstring,",left=1,top=2,width=8,height=5)")
            mydoc=eval(parse(text=temp))
       # }
    } else{
        temp=paste0("body_add_vg(mydoc,code=",plotstring,")")
        # mydoc <- mydoc %>%
        #     add_title(title)
        # if(echo) {
        #     mydoc<-mydoc %>% body_add_par(value="",style="Normal")
        #     codeft=Rcode2flextable(plotstring,eval=FALSE,format="docx")
        #     mydoc<-mydoc %>% body_add_flextable(codeft)
        # }
        mydoc=eval(parse(text=temp))
    }
    mydoc
}
