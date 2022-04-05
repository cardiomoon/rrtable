#' Add a flextable or mytable object into a document object
#' @param mydoc A document object
#' @param ftable A flextable or mytable object
#' @param code R code string
#' @param echo whether or not display R code
#' @param landscape Logical. Whether or not make a landscape section.
#' @importFrom officer add_slide ph_with body_add_par body_end_section_landscape body_end_section_portrait
#' @importFrom flextable body_add_flextable
#' @return a document object
#' @export
#' @examples
#' \dontrun{
#' require(rrtable)
#' require(moonBook)
#' require(officer)
#' require(magrittr)
#' ftable=mytable(Dx~.,data=acs)
#' title="mytable Example"
#' ft=df2flextable(head(iris))
#' title2="df2flextable Example"
#' doc=read_docx()
#' doc %>% add_text(title=title) %>%
#'         add_flextable(ftable) %>%
#'         add_text(title=title2) %>%
#'         add_flextable(ft)
#'}
add_flextable=function(mydoc,ftable,echo=FALSE,code="",landscape=FALSE){
     if("mytable" %in% class(ftable)){
          ft<-mytable2flextable(ftable)
     } else {
          ft<-ftable
     }
     pos=1.5
     if(echo & (code!="")) pos=2
     if(inherits(mydoc,"rpptx")){

              mydoc<-mydoc %>% ph_with(value=ft,location = ph_location(left=1,top=pos))
     } else {
          if(landscape) mydoc <- body_end_section_portrait(mydoc)
          mydoc<-mydoc %>% body_add_flextable(ft)
          if(landscape) mydoc <- body_end_section_landscape(mydoc)
     }

     mydoc
}


