

#' Make a data.frame with character strings encoding R code
#' @param result character strings encoding R code
#' @param preprocessing character strings encoding R code as a preprocessing
#' @param eval logical. Whether or not evaluate the code
#' @importFrom utils capture.output
Rcode2df=function(result,preprocessing,eval=TRUE){
    if(preprocessing!="") eval(parse(text=preprocessing))
    res=c()
    codes=unlist(strsplit(result,"\n",fixed=TRUE))
    for(i in 1:length(codes)){
        #if(codes[i]=="") next
        if(length(grep("cat",codes[i]))==1) {
            if(grep("cat",codes[i])==1) next
        }
        res=c(res,codes[i])
        if(eval){
            temp=capture.output(eval(parse(text=codes[i])))
            if(length(temp)==0) temp1=""
            else  {
                temp1=Reduce(pastelf,temp)
                temp1=paste0(temp1,"\n ")
            }
            res=c(res,temp1)
        }

    }
    data.frame(result=res,stringsAsFactors = FALSE)

}

pastelf=function(...){
    paste(...,sep="\n")
}

#' Split strings with desired length with exdent
#' @param string String
#' @param size desired length
#' @param exdent exdent
#' @importFrom stringr str_extract_all str_flatten str_pad
#' @export
#' @return splitted character vector
tensiSplit <- function(string,size=82,exdent=3) {
    if(!is.character(string)) {
        result<-string
    } else{
        result=c()
        if(nchar(string)<=size) {
            result=string
        } else{
            temp=substr(string,1,size)
            result=unlist(str_extract_all(substr(string,size+1,nchar(string)), paste0('.{1,',size-exdent,'}')))
            result=paste0(str_flatten(rep(" ",exdent)),result)
            result=c(temp,result)
        }
        result<-str_pad(result,size,"right")
    }
    result
}


#' Make a flextable with a data.frame
#' @param df A data.frame
#' @param bordercolor A border color name
#' @param format desired format. choices are "pptx" or "docx"
#' @param eval logical. Whether or not evaluate the code
#' @importFrom flextable delete_part flextable height_all void
#' @importFrom stringr str_split str_wrap
#' @return A flextable object
df2RcodeTable=function(df,bordercolor="gray",format="pptx",eval=TRUE){
    # df
    #bordercolor="gray";maxlen=80
    maxlen=ifelse(format=="pptx",92,82)
    font_size=ifelse(format=="pptx",11,10)
    no<-code<-c()
    for(i in 1:nrow(df)){
        temp=df[i,]
        result=unlist(strsplit(temp,"\n",fixed=TRUE))
        if(length(result)>0){
            for(j in 1:length(result)){

                splitedResult=tensiSplit(result[j],size=maxlen)
                code=c(code,splitedResult)
                no=c(no,rep(i,length(splitedResult)))
            }
        }
    }
    df2=data.frame(no,code,stringsAsFactors = FALSE)
    ft<- flextable(df2) %>%
         align(align="left",part="all") %>% border_remove()
    if(eval) {
        ft <-ft %>% bg(i=~no%%2==1,bg="#EFEFEF")
    } else{
        ft <-ft %>% bg(bg="#EFEFEF")
    }
    ft<- ft %>%
         padding(padding=0) %>%
         #padding(i=~no%%2==0,padding.left=10) %>%
         font(fontname="Monaco",part="all") %>%
         fontsize(size=font_size) %>%
         delete_part(part="header") %>%
         void(j=1) %>%
         autofit() %>% height_all(height=0.2,part="all")
    ft
}

#' Make a flextable object with character strings encoding R code
#' @param result character strings encoding R code
#' @param preprocessing character strings encoding R code as a preprocessing
#' @param format desired format. choices are "pptx" or "docx"
#' @param eval logical. Whether or not evaluate the code
#' @export
#' @examples
#' Rcode2flextable("str(mtcars)\nsummary(mtcars)",eval=FALSE)
Rcode2flextable=function(result,preprocessing="",format="pptx",eval=TRUE){

    df=Rcode2df(result,preprocessing=preprocessing,eval=eval)
    df2RcodeTable(df,format=format,eval=eval)

}



#' Make a R code slide into a document object
#' @param mydoc A document object
#' @param code  A character string encoding R codes
#' @param preprocessing A character string of R code as a preprocessing
#' @param format desired format. choices are "pptx" or "docx"
#' @return a document object
#' @export
#' @examples
#' library(rrtable)
#' library(magrittr)
#' library(officer)
#' code="summary(lm(mpg~hp+wt,data=mtcars))"
#' read_pptx() %>% add_text(title="Regression Analysis") %>%
#'    add_Rcode(code) %>% print(target=paste0(tempdir(),"/","test.pptx"))
add_Rcode=function(mydoc,code,preprocessing="",format="pptx"){

    ft <- Rcode2flextable(code,preprocessing=preprocessing,format=format)
    mydoc <- mydoc %>% add_flextable(ft)
    mydoc
}




