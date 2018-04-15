#' Add title to docx file
#' @param x A document object
#' @param title Title
#' @param size font size
#' @param color font color
#' @param before Whether or not add blank paragraph before title
#' @param after Whether or not add blank paragraph after title
#' @importFrom officer shortcuts fpar ftext body_add_fpar
#' @importFrom stats update
#' @export
add_title=function(x,title="",size=20,color=NULL,before=TRUE,after=TRUE){
    bold_face <- shortcuts$fp_bold(font.size = size)
    if(!is.null(color)) bold_face=update(bold_face,color=color)
    fpar1=fpar(ftext(title, prop = bold_face))
    if(before) x <- x %>% body_add_par("",style="Normal")
    x <- x %>% body_add_fpar(fpar1)
    if(after) x <- x %>% body_add_par("",style="Normal")
    x
}

#'add self data to document
#' @param mydoc A document object
#' @param data a data.frame
#' @export
add_self=function(mydoc,data){
    if(class(mydoc)=="rpptx"){
        mydoc <- mydoc %>% add_slide("Blank",master="Office Theme")
        mydoc<-mydoc %>% ph_with_flextable_at(value=df2flextable2(data),left=1,top=2)
    } else{
        mydoc<-mydoc %>% body_add_par(value="\n\n",style="Normal")
        #df=data.frame(title=title,text=text,code=code)
        mydoc<-mydoc %>% body_add_flextable(df2flextable2(data))
        mydoc<-mydoc %>% body_add_par(value="\n\n",style="Normal")
    }
    mydoc
}

#' Add text to document
#' @param mydoc A document object
#' @param title An character string as a plot title
#' @param text text string to be added
#' @param code An R code string
#' @param echo logical Whether or not show R code
#' @param eval logical whether or not evaluate the R code
#' @param landscape Logical. Whether or not make a landscape section.
#' @param style text style
#' @export
add_text=function(mydoc,title="",text="",code="",echo=FALSE,eval=FALSE,style="Normal",landscape=FALSE){
    if(class(mydoc)=="rpptx"){

         if(text!=""){
        mydoc <- mydoc %>%
            add_slide(layout = "Title and Content", master = "Office Theme") %>%
            ph_with_text(type="title",str=title) %>%
            ph_with_text(type="body",str=text)
        } else {
            mydoc <- mydoc %>%
                add_slide(layout = "Title Only", master = "Office Theme") %>%
                ph_with_text(type="title",str=title)
        }
        pos=1.5
        if(echo) {
            if(code!=""){
            codeft=Rcode2flextable(code,eval=eval,format="pptx")
            mydoc<-mydoc %>% ph_with_flextable_at(value=codeft,left=1,top=pos)
            pos=2
            }
        }


    } else{
        if(landscape) {
            mydoc <- mydoc %>% body_end_section(continuous = FALSE,landscape=FALSE)
        }
        mydoc <- mydoc %>% add_title(title)
        if(text!="") mydoc<-mydoc %>% body_add_par(value=text,style=style)
        if(echo) {
            if(code!=""){
            codeft=Rcode2flextable(code,eval=eval,format="docx")
            mydoc<-mydoc %>% body_add_par(value="\n\n",style="Normal")
            mydoc<-mydoc %>% body_add_flextable(codeft)
            mydoc<-mydoc %>% body_add_par(value="\n\n",style="Normal")
            }
        }

    }
    mydoc
}

#' Add two ggplots into a document object
#' @param mydoc A document object
#' @param plot1 An R code encoding the first ggplot
#' @param plot2 An R code encoding the second ggplot
#' @param width plot width in inches
#' @param height plot height in inches
#' @param top top plot position in inches
#' @return a document object
#' @importFrom officer body_end_section break_column_before
#' @export
#' @examples
#' require(ggplot2)
#' require(magrittr)
#' require(officer)
#' require(rvg)
#' plot1 <- "ggplot(data = iris, aes(Sepal.Length, Petal.Length)) + geom_point()"
#' plot2 <- "ggplot(data = iris, aes(Sepal.Length, Petal.Length, color = Species)) + geom_point()"
#' read_pptx() %>% add_text(title="Two ggplots") %>% add_2ggplots(plot1=plot1,plot2=plot2)
add_2ggplots=function(mydoc,plot1,plot2,width=3,height=2.5,top=2){
    gg1<-eval(parse(text=plot1))
    gg2<-eval(parse(text=plot2))

    if(class(mydoc)=="rpptx"){

        mydoc<- mydoc %>%
            ph_with_vg_at(code = print(gg1), left=0.5,top=top,width=4.5,height=5 ) %>%
            ph_with_vg_at(code = print(gg2), left=5,top=top,width=4.5,height=5 )


    } else{

        mydoc <- mydoc %>%
            body_end_section(continuous = TRUE)
        mydoc <-mydoc %>%
            body_add_vg(code=print(gg1),width=width,height=height) %>%
            body_add_vg(code=print(gg2),width=width,height=height) %>%
            body_end_section(continuous = TRUE,
                             colwidths = c(.5, .5), space = .05, sep = FALSE)
    }
    mydoc

}
#' Add two plots into a document object
#' @param mydoc A document object
#' @param plotstring1 An R code string encoding the first plot
#' @param plotstring2 An R code string encoding the second plot
#' @param width plot width in inches
#' @param height plot height in inches
#' @param echo logical Whether or not show R code
#' @param top top plot position in inches
#' @return a document object
#' @export
#' @examples
#' require(magrittr)
#' require(officer)
#' plotstring1="plot(1:10)"
#' plotstring2="hist(rnorm(100))"
#' read_pptx() %>% add_text(title="Two plots") %>% add_2plots(plotstring1,plotstring2) %>%
#' print(target="demo.pptx")
add_2plots=function(mydoc,plotstring1,plotstring2,width=3,height=2.5,echo=FALSE,top=2){

    if(class(mydoc)=="rpptx"){

        temp1=paste0("ph_with_vg_at(mydoc,code=",plotstring1,",left=0.5,top=top,width=4.5,height=5 )")
        temp2=paste0("ph_with_vg_at(mydoc,code=",plotstring2,",left=5,top=top,width=4.5,height=5)")
        mydoc=eval(parse(text=temp1))
        mydoc=eval(parse(text=temp2))

    } else{
        temp1=paste0("body_add_vg(mydoc,code=",plotstring1,
                     ",width=",width,",height=",height,")")
        temp2=paste0("body_add_vg(mydoc,code=",plotstring2,
                     ",width=",width,",height=",height,")")

        mydoc <- mydoc %>%
            body_end_section(continuous = TRUE)

        mydoc=eval(parse(text=temp1))
        mydoc=eval(parse(text=temp2))
        mydoc=body_end_section(mydoc,continuous = TRUE,
                         colwidths = c(.5, .5), space = .05, sep = FALSE)
    }
    mydoc
}


getCodeOption=function(x,what="echo"){
    result=FALSE
    x=unlist(strsplit(x,","))
    x=x[str_detect(x,what)]
    if(length(x)>0){
        x=unlist(strsplit(x,"="))[2]
        result=eval(parse(text=x))
    }
    result
}
# plotstring1="plot(1:10)"
# plotstring2="hist(rnorm(100))"
#
# read_docx() %>% add_2plots(plotstring1="plot(1:10)",plotstring2="hist(rnorm(100))",title="Two plots") %>% print(target="test.docx")

# gg1 <- ggplot(data = iris, aes(Sepal.Length, Petal.Length)) +
#     geom_point()
# gg2 <- ggplot(data = iris, aes(Sepal.Length, Petal.Length, color = Species)) +
#     geom_point()
#
# text1="This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.
# When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:"
#
# my_doc <- read_pptx()  %>%
#     add_text("Text1",text=text1) %>%
#     add_2ggplots(title="Two plots",plot1=gg1,plot2=gg2) %>%
#     add_text("Text2",text=text1)
#
# print(my_doc, target = "section.pptx")
