#' Add title slide
#' @param mydoc A document object
#' @param title An character string as a title
#' @param subtitle An character string as a subtitle
#' @export
#' @examples
#' require(magrittr)
#' require(officer)
#' read_pptx() %>% add_title_slide(title="Web-based analysis with R")
add_title_slide=function(mydoc,title="",subtitle=""){
    mydoc <- mydoc %>%
        add_slide(layout="Title Slide",master="Office Theme") %>%
        ph_with_text(type="ctrTitle",str=title) %>%
        ph_with_text(type="subTitle",str=subtitle)
    mydoc
}
