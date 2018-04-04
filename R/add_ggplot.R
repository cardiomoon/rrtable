#' Add ggplot into a document object
#' @param mydoc A document object
#' @param title An character string as a plot title
#' @param code R code for table
#' @param echo logical Whether or not show R code
#' @return a document object
#' @importFrom rvg ph_with_vg body_add_vg
#' @export
#' @examples
#' require(rrtable)
#' require(ggplot2)
#' require(officer)
#' require(magrittr)
#' code <- "ggplot(mtcars, aes(x = mpg , y = wt)) + geom_point()"
#' read_pptx() %>% add_ggplot(code=code,echo=TRUE)
add_ggplot=function(mydoc,title="",code="",echo=FALSE){
    if(class(mydoc)=="rpptx"){
        mydoc<- mydoc %>%
            add_slide(layout = "Title and Content", master = "Office Theme") %>%
            ph_with_text(type="title",str=title)

        if(echo){
            codeft=Rcode2flextable(code,eval=FALSE,format="pptx")
            mydoc<-mydoc %>% ph_with_flextable_at(value=codeft,left=1,top=2)
            temp=paste0("ph_with_vg_at(mydoc,code=print(",code,"),left=1,top=2.3,width=8,height=5)")
            mydoc=eval(parse(text=temp))
        } else{
            temp=paste0("ph_with_vg_at(mydoc,code=print(",code,"),left=1,top=2,width=8,height=5)")
            mydoc=eval(parse(text=temp))
        }

    } else{
        mydoc <- mydoc %>%
            add_title(title)
        temp=paste0("body_add_vg(mydoc,code=print(",code,"))")
        if(echo) {
            mydoc<-mydoc %>% body_add_par(value="",style="Normal")
            codeft=Rcode2flextable(code,eval=FALSE,format="docx")
            mydoc<-mydoc %>% body_add_flextable(codeft)
        }
        mydoc=eval(parse(text=temp))
    }
    mydoc
}
