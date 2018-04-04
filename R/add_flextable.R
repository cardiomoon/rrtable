#' Add a flextable or mytable object into a document object
#' @param mydoc A document object
#' @param ftable A flextable or mytable object
#' @param title An character string as a plot title
#' @param code R code for table
#' @param echo logical Whether or not show R code
#' @importFrom officer add_slide ph_with_text  body_add_par
#' @importFrom flextable body_add_flextable ph_with_flextable ph_with_flextable_at
#' @return a document object
#' @export
#' @examples
#' require(rrtable)
#' require(moonBook)
#' require(officer)
#' require(magrittr)
#' ftable=mytable(Dx~.,data=acs)
#' title="mytable Example"
#' ft=df2flextable(head(iris))
#' title2="df2flextable Example"
#' doc=read_docx()
#' doc %>% add_flextable(ftable,title,code="mytable(Dx~.,data=acs)",echo=TRUE) %>%
#'         add_flextable(ft,title2,code="df2flextable(head(iris))",echo=TRUE) %>%
#'         print(target="mytable.docx")
#' read_pptx() %>%
#'        add_flextable(ftable,title,code="mytable(Dx~.,data=acs)",echo=TRUE) %>%
#'        add_flextable(ftable,title,code="mytable(Dx~.,data=acs)") %>%
#'        add_flextable(ft,title2,code="df2flextable(head(iris))",echo=TRUE) %>%
#'        add_flextable(ft,title2,code="df2flextable(head(iris))") %>%
#'        print(target="mytable.pptx")
add_flextable=function(mydoc,ftable,title="",code="",echo=FALSE){
     if("mytable" %in% class(ftable)){
          ft<-mytable2flextable(ftable)
     } else {
          ft<-ftable
     }
     if(class(mydoc)=="rpptx"){
          mydoc <- mydoc %>% add_slide("Title and Content",master="Office Theme")
          mydoc <- mydoc %>% ph_with_text(type="title",str=title)
          if(echo) {
              codeft=Rcode2flextable(code,eval=FALSE,format="pptx")
              mydoc<-mydoc %>% ph_with_flextable_at(value=codeft,left=1,top=2)
              mydoc<-mydoc %>% ph_with_flextable_at(value=ft,left=1,top=2.5)
          } else{
              mydoc<-mydoc %>% ph_with_flextable_at(value=ft,left=1,top=2)
          }

     } else {
          mydoc <- mydoc %>% add_title(title)
          mydoc<-mydoc %>% body_add_par(value="",style="Normal")
          if(echo) {
              codeft=Rcode2flextable(code,eval=FALSE,format="docx")
              mydoc<-mydoc %>% body_add_flextable(codeft)
              mydoc<-mydoc %>% body_add_par(value="",style="Normal")
          }
          mydoc<-mydoc %>% body_add_flextable(ft)
     }
     mydoc
}





