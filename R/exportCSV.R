#' Export pptxList file to desired format
#' @param file The name of the file which the data are to be read from.
#' @param format desired output format. Possible choices are one of the c("HTML","pdf","word","pptx","plotzip")
#' @param rawDataName The name of the rawData
#' @param rawDataFile The name of the rawData file which the data are to be read from.
#' @export
exportCSV=function(file,format="HTML",rawDataName=NULL,rawDataFile="rawData.RDS"){
    if(!is.null(rawDataName)){
        rawData=readRDS(rawDataFile)
        assign(rawDataName,rawData)
    }
    data<-readr::read_csv(file,comment="#")
    preprocessing<-rrtable::readComment(file)
    temp=paste0("rrtable::data2",format,'(data,preprocessing="',preprocessing,'",rawDataName="',rawDataName,"\")")
    print(temp)
    eval(parse(text=temp))
}
