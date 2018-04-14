#' Add ggplot into a document object
#' @param mydoc A document object
#' @param code R code for table
#' @param top top position of plot
#' @return a document object
#' @importFrom rvg ph_with_vg body_add_vg
#' @export
#' @examples
#' require(rrtable)
#' require(ggplot2)
#' require(officer)
#' require(magrittr)
#' code <- "ggplot(mtcars, aes(x = mpg , y = wt)) + geom_point()"
#' read_pptx() %>% add_text(title="ggplot") %>% add_ggplot(code=code)
add_ggplot=function(mydoc,code="",top=2){
    if(class(mydoc)=="rpptx"){

            temp=paste0("ph_with_vg_at(mydoc,code=print(",code,"),left=1,top=",top,",width=8,height=5)")
            mydoc=eval(parse(text=temp))

    } else{
        temp=paste0("body_add_vg(mydoc,code=print(",code,"))")

        mydoc=eval(parse(text=temp))
    }
    mydoc
}
