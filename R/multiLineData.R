result=editData::editData(sampleData3)


sampleData3=result
result
data2HTML(result)

require(rrtable)
df2flextable(sampleData3)

colnames(sampleData3)

multiLineData=function(data,colno=4){
       multi=multiLineCount(data,colno)
       if(length(multi)==0) {
           data1=data
       } else{
           multi2<-c()
           multi3<-multi
           k=1
           data1<-data[0,]
           rowlist=list()
           for(i in 1:nrow(data)){
                if(i %in% multi3){
                    x=unlist(strsplit(data[[colno]][i],"\n"))
                    data2=data[0,]
                    for(j in 1:length(x)){
                        data2=rbind(data2,data[i,])
                    }
                    data2[2:length(x),1:ncol(data2)]=""
                    data2[[colno]]=x
                    data1=rbind(data1,data2)
                    multi2=c(multi2,multi[k]:(multi[k]+length(x)-1))
                    rowlist[[k]]=multi[k]:(multi[k]+length(x)-1)
                    cat("Before\nmulti=",multi,"\n")
                    multi[(k+1):length(multi)]=multi[(k+1):length(multi)]+(length(x)-1)
                    cat("After\nmulti=",multi,"\n")
                    k=k+1
                } else{
                    data1=rbind(data1,data[i,])
                }
           }
       }
       cat("multi2=",multi2,"\n")
       cat("rowlist\n")
       str(rowlist)
       cat("\n")
       ft=df2flextable(data1)
       y=setdiff(1:ncol(data1),colno)  #columnes to be merged
       for(j in seq_along(y)){
           for(k in 1:length(rowlist))
              ft=flextable::merge_at(ft,i=rowlist[[k]],y[j])
       }
       ft
}


multiLineCount=function(data,colno){
    x=data[[colno]]
    result=c()
    for(i in seq_along(x)){
        if(length(unlist(strsplit(x[i],"\n")))>1){
            result=c(result,i)
        }
    }
    result
}


data=sampleData3
colno=4
multiLineCount(data,colno)

multiLineData(data,4)
