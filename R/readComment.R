#' Read comment from a file
#' @param filename A path for destination file
#' @param comment A string used to identify comments
#' @export
readComment=function(filename,comment="#"){
    res=readLines(filename,1)
    res
    count=0
    if(!is.na(unlist(strsplit(res,comment,fixed=TRUE))[2])){
        count=as.numeric(unlist(strsplit(res,"#"))[2])
    }
    count
    if(count==0) {
        result=""
    } else{
        result=readLines(filename,count)
        result
        result=result[-1]
        result=gsub(comment,"",result)
        result=paste(result,collapse="\n")
    }
    result
}
