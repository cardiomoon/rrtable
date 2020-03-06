#' Add title to docx file
#' @param x A document object
#' @param title Title
#' @param size font size
#' @param color font color
#' @param before Whether or not add blank paragraph before title
#' @param after Whether or not add blank paragraph after title
#' @importFrom officer shortcuts fpar ftext body_add_fpar ph_location_type ph_location
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
        mydoc<-mydoc %>% ph_with(value=df2flextable2(data), location = ph_location(left=1,top=2))
    } else{
        mydoc<-mydoc %>% body_add_par(value="\n\n",style="Normal")
        #df=data.frame(title=title,text=text,code=code)
        mydoc<-mydoc %>% body_add_flextable(df2flextable2(data))
        mydoc<-mydoc %>% body_add_par(value="\n\n",style="Normal")
    }
    mydoc
}

#' Add hyperlink text
#' @param mydoc A document object
#' @param text text string to be added
#' @importFrom stringr str_extract str_remove
#' @importFrom officer ph_add_text
add_text2hyperlink=function(mydoc,text){


    if(str_detect(text,"\\]\\(")){
        devide=function(x){
            ref=str_extract(x,"\\(.*\\)")
            x=str_remove(x,"\\(.*\\)")
            str=str_extract(x,"\\[.*\\]")
            text=str_remove(x,"\\[.*\\]")
            str=substr(str,2,nchar(str)-1)
            ref=substr(ref,2,nchar(ref)-1)
            list(text=text,str=str,ref=ref)
        }
        temp=str_extract_all(text,".*?\\[.*?\\]\\(.*?\\)")
        result=lapply(temp,devide)

        for(i in 1:length(result[[1]]$text)){
            if(i==1) {
                mydoc=ph_with(mydoc,value=result[[1]]$text[i],location = ph_location_type(type="body"))
            } else{
                mydoc=ph_add_text(mydoc,type="body",str=result[[1]]$text[i])
            }

           mydoc=ph_add_text(mydoc,type="body",str=result[[1]]$str[i],href=result[[1]]$ref[i])

        }


    } else{
        mydoc=ph_with(mydoc, text, location = ph_location_type(type="body"))
    }
    mydoc
}

#' Add text to document
#' @param mydoc A document object
#' @param title An character string as a plot title
#' @param text text string to be added
#' @param code An R code string
#' @param preprocessing preprocessing
#' @param echo logical Whether or not show R code
#' @param eval logical whether or not evaluate the R code
#' @param landscape Logical. Whether or not make a landscape section.
#' @param style text style
#' @importFrom officer body_end_section_portrait
#' @export
add_text=function(mydoc,title="",text="",code="",preprocessing="",echo=FALSE,eval=FALSE,style="Normal",landscape=FALSE){
    if(class(mydoc)=="rpptx"){
        layout="Title and Content"
        if((title=="")&(text=="")) layout="Blank"
        else if(text=="") layout="Title Only"

        mydoc <- mydoc %>%
            add_slide(layout = layout, master = "Office Theme")


        if(title!=""){
           mydoc <- mydoc %>%
              ph_with(value=title, location = ph_location_type(type="title"))
        }
        if(text!=""){

            mydoc <- mydoc %>%
                add_text2hyperlink(text=text)
        }
        pos=1.5
        if(echo) {
            if(code!=""){
            codeft=Rcode2flextable(code,preprocessing=preprocessing,eval=eval,format="pptx")
            mydoc<-mydoc %>% ph_with(value=codeft, location = ph_location(left=1,top=pos))
            pos=2
            }
        }


    } else{
        if(landscape) {
            mydoc <- mydoc %>% body_end_section_portrait()
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
#' @param preprocessing preprocessing
#' @param width plot width in inches
#' @param height plot height in inches
#' @param top top plot position in inches
#' @return a document object
#' @importFrom officer body_end_section_columns body_end_section_continuous
#' @export
#' @examples
#' \donttest{
#' require(ggplot2)
#' require(magrittr)
#' require(officer)
#' require(rvg)
#' plot1 <- "ggplot(data = iris, aes(Sepal.Length, Petal.Length)) + geom_point()"
#' plot2 <- "ggplot(data = iris, aes(Sepal.Length, Petal.Length, color = Species)) + geom_point()"
#' read_pptx() %>% add_text(title="Two ggplots") %>% add_2ggplots(plot1=plot1,plot2=plot2)
#' read_docx() %>% add_text(title="Two ggplots") %>% add_2ggplots(plot1=plot1,plot2=plot2)
#' }
add_2ggplots=function(mydoc,plot1,plot2,preprocessing="",width=3,height=2.5,top=2){

    if(preprocessing!="") {
        eval(parse(text=preprocessing))
    }

    gg1<-eval(parse(text=plot1))
    gg2<-eval(parse(text=plot2))

    if(class(mydoc)=="rpptx"){

        mydoc<- mydoc %>%
            ph_with(dml(code = print(gg1)), location = ph_location(left=0.5,top=top,width=4.5,height=5) ) %>%
            ph_with(dml(code = print(gg2)), location = ph_location(left=5,top=top,width=4.5,height=5 ))


    } else{

        mydoc <- mydoc %>%
            body_end_section_continuous()
        mydoc <-mydoc %>%
            body_add_gg(value=gg1,width=width,height=height) %>%
            body_add_gg(value=gg2,width=width,height=height) %>%
            body_end_section_columns(widths = c(width, width), space = .05, sep = FALSE)
    }
    mydoc

}

#' Add two flextables into a document object
#' @param mydoc A document object
#' @param ft1 The first flextable
#' @param ft2 The second flextable
#' @param echo whether or not display R code
#' @param width plot width in inches
#' @param code R code string
#' @return a document object
#' @importFrom officer slip_in_column_break body_add_gg
#' @export
#' @examples
#' \donttest{
#' require(rrtable)
#' require(officer)
#' require(magrittr)
#' title="Two Tables"
#' ft1=df2flextable(head(iris[1:4]))
#' ft2=df2flextable(tail(iris[1:4]))
#' doc=read_docx()
#' doc %>% add_text(title=title) %>%
#'         add_2flextables(ft1,ft2)
#' doc=read_pptx()
#' doc %>% add_text(title=title) %>%
#'         add_2flextables(ft1,ft2)
#'}
add_2flextables=function(mydoc,ft1,ft2,echo=FALSE,width=3,code=""){

    pos=1.5
    if(echo & (code!="")) pos=2
    if(class(mydoc)=="rpptx"){

        mydoc<-mydoc %>%
            ph_with(value=ft1, location = ph_location(left=0.5,top=pos)) %>%
            ph_with(value=ft2, location = ph_location(left=5,top=pos))
    } else {

        # if(landscape) mydoc <- body_end_section_portrait(mydoc)

        mydoc <- mydoc %>%
            body_end_section_continuous()
        mydoc <-mydoc %>%
            body_add_flextable(value=ft1) %>%
            body_add_flextable(value=ft2) %>%
            slip_in_column_break() %>%
            body_end_section_columns()
        # if(landscape) mydoc <- body_end_section_landscape(mydoc)
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



