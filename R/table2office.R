#'Export data.frame or statistical output to a table in Microsoft Office
#' @param x An object or string
#' @param target name of output file
#' @param append logical value
#' @param title Optional character of plot title
#' @param vanilla A logical
#' @param echo logical
#' @param preprocessing A character string
#' @param type "pptx" or "docx"
#' @param landscape logical
#' @param left left margin
#' @param top top margin
#' @importFrom moonBook mytable
#' @export
table2office=function(x=NULL,target="Report",append=FALSE,title="",vanilla=FALSE,echo=FALSE,
                      preprocessing="",type="pptx",landscape=FALSE,left=1,top=1){

    doc<-open_doc(target=target,type=type,append=append)
    target=attr(doc,"name")
    if(title!=""){
        doc <- doc %>% add_text(title=title)

    } else {
        if(type=="pptx") doc <- doc %>% add_slide(layout="Blank")
    }
    pos=top
    if(title!="") pos=pos+0.5
    if(echo & is.character(x)) {

        codeft=Rcode2flextable(x,preprocessing=preprocessing,format="pptx",eval=FALSE)
        doc<-doc %>% ph_with(value=codeft, location = ph_location(left=1,top=pos))
        pos=pos+0.5

    }

    if("character" %in% class(x)){
        x<-eval(parse(text=x))
    }
    if("mytable" %in% class(x)){
        ft<-mytable2flextable(x,vanilla=vanilla)
    } else if(class(x)[1] %in%
              c("matrix","lm","fitdistr","nls","aov","anova","glm","coxph","prcomp","summary.prcomp")){
        ft<-ztable2flextable(ztable(x),vanilla=vanilla)
    } else if(class(x)[1]=="data.frame"){
        ft<-df2flextable(x,vanilla=vanilla)
    } else if(class(x)[1]=="flextable"){
        ft<-x
    }
    if(class(doc)=="rpptx"){
        doc<-doc %>% ph_with(value=ft,location = ph_location(left=left,top=pos))
    } else {
        if(landscape) doc <- body_end_section_portrait(doc)
        doc<-doc %>% body_add_flextable(ft)
        if(landscape) doc <- body_end_section_landscape(doc)
    }
    message(paste0("Exported table as ", target))
    doc %>% print(target=target)
}

#' Export data.frame or statistical output to Microsoft Powerpoint format
#' @param ... further arguments to be passed to table2office
#' @export
#' @examples
#' \donttest{
#' require(moonBook)
#' x="mytable(Dx~.,data=acs)"
#' table2pptx(x,title="mytable object",echo=TRUE)
#' table2pptx("head(iris)",title="data.Frame",append=TRUE,vanilla=FALSE,echo=TRUE)
#' x="fit<-lm(mpg~wt*hp,data=mtcars);fit"
#' table2pptx(x,title="Linear regression",append=TRUE,vanilla=TRUE,echo=TRUE)
#' fit2="aov(yield ~ block + N * P + K, data = npk)"
#' table2pptx(fit2,title="ANOVA",append=TRUE,vanilla=TRUE,echo=TRUE)
#' }
table2pptx=function(...){
    table2office(...,type="pptx")
}


#' Export data.frame or statistical output to Microsoft Word format
#' @param ... further arguments to be passed to table2office
#' @export
#' @examples
#' \donttest{
#' require(moonBook)
#' x=mytable(Dx~.,data=acs)
#' table2docx(x)
#' table2docx(head(iris),title="head(iris)",append=TRUE,vanilla=FALSE)
#' fit=lm(mpg~wt*hp,data=mtcars)
#' table2docx(fit,title="Linear regression",append=TRUE,vanilla=TRUE)
#' fit2=aov(yield ~ block + N * P + K, data = npk)
#' table2docx(fit2,title="Linear regression",append=TRUE,vanilla=TRUE)
#' }
table2docx=function(...){
    table2office(...,type="docx")
}
