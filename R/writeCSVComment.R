#' Write a csv file with comment
#' @param data A data.frame
#' @param file A path for destination file
#' @param metadata A character string representing R codes as a preprocessing
#' @param comment A string used to identify comments
#' @importFrom utils write.table
#' @export
writeCSVComment=function(data,file,metadata="",comment="#"){
    if(metadata!=""){
        count=length(unlist(strsplit(metadata,"\n")))+1
        temp=paste0(comment,unlist(strsplit(metadata,"\n")))
        temp=paste(temp,collapse="\n")
        temp=paste0(comment,count,"\n",temp)
        temp
        writeLines(text=temp,con=file)
    }
    # append the data.frame
    write.table(data, file = file, append = T, row.names = F, sep = ',',col.names=TRUE)
}

#' Read a csv file with comment
#' @param file A path for destination file
#' @export
readCSVComment=function(file){
    data=readr::read_csv(file,comment="#")
    data[is.na(data)]=""
    data
}


