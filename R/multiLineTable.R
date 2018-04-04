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
#'@export
df2flextable2=function(data){
    multiLineTable(data)
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
    temp1=unlist(strsplit(x,"\n"))
    unlist(strsplit(temp1,";"))
}


