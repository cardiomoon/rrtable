#' Add two plots into a document object
#' @param mydoc A document object
#' @param plotstring1 An R code string encoding the first plot
#' @param plotstring2 An R code string encoding the second plot
#' @param preprocessing preprocessing
#' @param plottype character  One of c("auto","plot","ggplot")
#' @param width plot width in inches
#' @param height plot height in inches
#' @param echo logical Whether or not show R code
#' @param top top plot position in inches
#' @return a document object
#' @export
#' @examples
#' require(magrittr)
#' require(officer)
#' require(ggplot2)
#' plotstring1="plot(iris)"
#' plotstring2="ggplot(iris,aes(x=Sepal.Length,y=Sepal.Width))+geom_point()"
#' read_pptx() %>% add_text(title="Two plots") %>% add_2plots(plotstring1,plotstring2)
#' read_docx() %>% add_text(title="Two plots") %>% add_2plots(plotstring1,plotstring2)
add_2plots=function(mydoc,plotstring1,plotstring2,preprocessing="",
                    plottype="auto",width=NULL,height=NULL,echo=FALSE,top=2){
    if(preprocessing!="") {
        eval(parse(text=preprocessing))
    }

    if(class(mydoc)=="rdocx"){
        mydoc <- mydoc %>%
            body_end_section_continuous()
        if(is.null(width)) width<-2.5
        if(is.null(height)) height<-3
    } else{
        if(is.null(width)) width<-4.5
        if(is.null(height)) height<-5
    }
    mydoc<-mydoc %>%
        add_anyplot(x=plotstring1,preprocessing=preprocessing,plottype=plottype,left=0.5,top=top,width=width,height=height) %>%
        add_anyplot(x=plotstring2,preprocessing=preprocessing,plottype=plottype,left=5,top=top,width=width,height=height)
    if(class(mydoc)=="rdocx"){
        mydoc <- mydoc %>%
            slip_in_column_break() %>%
            body_end_section_columns()
    }

    mydoc
}


