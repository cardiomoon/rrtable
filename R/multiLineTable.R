# result=editData::editData(sampleData3)
#
# sampleData3=result
# result
# sampleData3
# data2HTML(sampleData3)
#
# devtools::use_data(sampleData3,overwrite=TRUE)
#
# require(rrtable)
# df2flextable(sampleData3)
#
# colnames(sampleData3)

#'make multiline table
#'@param data a data.frame
#'@param ... further arguments to be passed to multiLineTable()
#'@export
df2flextable2=function(data,...){
    multiLineTable(data,...) %>% autofit()
}

#'make multiline data
#'@param data a data.frame
#'@param colno column number to separate
#'@export
multiLineData=function(data,colno=4){
    # data=head(mtcars)
    # colno=4
    if(ncol(data)<4) colno=3
    multi=multiLineCount(data,colno)

    if(length(multi)==0) {
        data1=data
    } else{

        data1<-data[0,]

        for(i in 1:nrow(data)){
            if(i %in% multi){
                x=separateLF(data[[colno]][i])
                data2=data[0,]
                for(j in 1:length(x)){
                    data2=rbind(data2,data[i,])
                }
                data2[2:length(x),1:ncol(data2)]=""
                data2[[colno]]=x
                data1=rbind(data1,data2)


            } else{
                data1=rbind(data1,data[i,])
            }
        }
    }
    data1
}


#'Make flextable with multiline
#'@param data a data.frame
#'@param colno column number to separate
#'@return a flextable
#'@export
#'@examples
#'multiLineTable(sampleData3,4)
multiLineTable=function(data,colno=4){
       multi=multiLineCount(data,colno)

       if(length(multi)==0) {
           data1=data
       } else{
           multi2<-c()
           multi3<-multi
           k=1
           data1<-data[0,]
           rowlist=list()
           current=1
           rowno=c()
           for(i in 1:nrow(data)){
                if(i %in% multi3){
                    x=separateLF(data[[colno]][i])
                    data2=data[0,]
                    for(j in 1:length(x)){
                        data2=rbind(data2,data[i,])
                    }
                    data2[2:length(x),1:ncol(data2)]=""
                    data2[[colno]]=x
                    data1=rbind(data1,data2)
                    multi2=c(multi2,multi[k]:(multi[k]+length(x)-1))
                    rowlist[[k]]=multi[k]:(multi[k]+length(x)-1)

                    multi[(k+1):length(multi)]=multi[(k+1):length(multi)]+(length(x)-1)

                    k=k+1
                    rowno=c(rowno,rep(current,length(x)))

                } else{
                    data1=rbind(data1,data[i,])
                    rowno=c(rowno,current)

                }
               current=!current
           }
       }

       ft=df2flextable(data1)

       if(length(multi)>0) {


       y=setdiff(1:ncol(data1),colno)  #columnes to be merged
       for(j in seq_along(y)){
           for(k in 1:length(rowlist))
              ft=flextable::merge_at(ft,i=rowlist[[k]],y[j])
       }
       ft=flextable::bg(ft,rowno==1,j=1:ncol(data1),bg="transparent")
       ft=flextable::bg(ft,rowno==0,j=1:ncol(data1),bg="#EFEFEF")
       ft = ft %>% align(align="left",part="body")
       }
       ft
}


#'Count multiline in a data.frame
#'@param data a data.frame
#'@param colno column number to separate
multiLineCount=function(data,colno){
    x=data[[colno]]
    result=c()
    for(i in seq_along(x)){
        if(length(separateLF(x[i]))>1){
            result=c(result,i)
        }
    }
    result
}

#'Split the linefeed and semicolon
#'@param x string
separateLF=function(x){
    if(!is.character(x)) {
        result=x
    } else{
        temp1=unlist(strsplit(x,"\n"))
        result=unlist(strsplit(temp1,";"))
    }
    result
}


# sampleData4 <- sampleData3 %>%
#     multiLineData(colno=4) %>%
#     adjustWidth() %>%
#     as.data.frame()


lfCount=function(data){
    result=c()
    for(i in 1:ncol(data)){
        temp=str_replace_all(data[[i]],"\\. ","\\.\n")
        result=c(result,length(unlist(strsplit(temp,"\n"))))
    }
    result
}

#' Seperate linefeed as a separate row in a data.frame
#' @param data A data.frame
#' @export
lfData=function(data){
    result<-data1<-data[0,]
    rowno=c()
    for(i in 1:nrow(data)){
        data1<-data[i,]
        maxno=max(lfCount(data1))
        maxno
        if(maxno==1){
            result=rbind(result,data1)
            rowno=c(rowno,i)
        } else{
            data2=stretchData(data1,max=maxno)
            data2
            result=rbind(result,data2)
            rowno=c(rowno,rep(i,nrow(data2)))
        }
    }
    result$rowno=rowno
    result
}


#'@importFrom purrr map_df
stretchData=function(data,max=4){

    result=data[0,]

    stretchCol=function(x,max=max){
        temp=str_replace_all(x,"\\. ","\\.\n")
        result=unlist(strsplit(temp,"\n"))
        if(length(result)<max) result=c(result,rep("",max-length(result)))
        result
    }

    as.data.frame(map_df(data,stretchCol,max=max))
}

lfTable=function(data){
    data1=lfData(data)
    rowno<-data1$rowno
    data1=data1[-ncol(data1)]
    df2flextable(data1) %>%
        flextable::bg(rowno %% 2 ==1,j=1:ncol(data1),bg="transparent") %>%
        flextable::bg(rowno %% 2 ==0,j=1:ncol(data1),bg="#EFEFEF") %>%
        align(align="left",part="body")
}
# require(rrtable)
# require(purrr)
# require(flextable)
# lfTable(sampleData3)
# x="You can insert the result of R code. For example, you can insert the result of regression analysis."
# str_replace_all(x,"\\. ","\\.\n")
