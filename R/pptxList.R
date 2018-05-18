#' Sample data for pptxList
#' A dataset containing five objects for reproducible research
#'
#' @format A data frame with 5 rows and three columns
#' \describe{
#'    \item{type}{type of data}
#'    \item{title}{title of data}
#'    \item{code}{R code of data}
#' }
"sampleData2"

#' Sample data for pptxList
#' A dataset containing five objects for reproducible research
#'
#' @format A data frame with 5 rows and three columns
#' \describe{
#'    \item{type}{type of data}
#'    \item{title}{title of data}
#'    \item{text}{text}
#'    \item{code}{R code of data}
#'    \item{option}{option for R code}
#' }
"sampleData3"


#' Server function of pptxList shiny module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param data A data object
#' @param preprocessing A character string of R code
#' @importFrom shiny reactiveValues updateTextAreaInput reactive h4 fileInput actionButton downloadButton hr p callModule
#' @importFrom shiny downloadHandler tagList conditionalPanel uiOutput observe observeEvent column fluidRow renderUI
#' @importFrom shiny htmlOutput imageOutput plotOutput renderImage renderPlot renderPrint tableOutput updateCheckboxInput
#' @importFrom editData numericInput3 radioButtons3 editableDT editableDTUI selectInput3
#' @importFrom readr read_csv
#' @export
pptxList<-function(input,output,session,data=reactive(""),preprocessing=reactive(""))
{

     savedPPT=reactiveValues()

     observe({
          if(data()!=""){
               df<-data()
               savedPPT$type=df$type
               savedPPT$title=df$title
               savedPPT$code=df$code
          } else{
               savedPPT$type=c()
               savedPPT$title=c()
               savedPPT$code=c()
          }
     })
     observe({
          if(preprocessing()!=""){
               updateTextAreaInput(session,"preprocessing",value=preprocessing())
          }
     })

     pptdf=reactive({

          input$pptfile
          input$resetPPT

          df<-data.frame(type=savedPPT$type,title=savedPPT$title,code=savedPPT$code,
                         stringsAsFactors = FALSE)
          df
     })

     observeEvent(input$pptfile,{
          if(!is.null(input$pptfile)){

               mypptlist<-readr::read_csv(input$pptfile$datapath,comment="#")
               savedPPT$type=mypptlist$type
               savedPPT$title=mypptlist$title
               savedPPT$code=mypptlist$code
               result=readComment(input$pptfile$datapath)
               if(result!=""){
                    updateTextAreaInput(session,"preprocessing",value=result)
               }
          }
     })

     mydf=reactive({

          data()
     })
     # output$text1=renderPrint({
     #         pptdf2()
     # })
     output$pptListUI=renderUI({
          ns <- session$ns
          pptList=pptdf()
          count=nrow(pptList)
          tagList(

               h4("Saved PowerPoint List"),
               if(count>0) editableDTUI(ns("PPTxListTable")),
               hr(),
               fluidRow(
                    column(4,
                           h4("Upload PPTList(*.csv)"),
                           fileInput(ns("pptfile"),NA)  #"Upload PPTList(*.csv)"
                    ),
                    column(3,
                           h4("Load sampleData "),
                           actionButton(ns("loadSample"),"load sampleData")
                    ),
                    column(5,
                           h4("Reset PPT List "),
                           actionButton(ns("ResetPPT"),"reset PPT List")
                    )
               ),

               if(count>0) checkboxInput(ns("showCode"),"show Code"),
               if(count>0) hr(),
               if(count>0) downloadButton(ns("savePPTxList"),"download as csv"),
               if(count>0) downloadButton(ns("downloadPPTxHTML"),"download as HTML"),
               if(count>0) downloadButton(ns("downloadPPTxPDF"),"download as PDF"),
               if(count>0) downloadButton(ns("downloadPPTxList"),"download as PPTx"),
               if(count>0) downloadButton(ns("downloadPPTxListWord"),"download as Word"),
               if(count>0) hr(),
               if(count>0) downloadButton(ns("downloadPlots"),"download Plots"),
               if(count>0) numericInput3(ns("plotRes"),"Resolution",value=300,step=1,width=80),
               if(count>0) selectInput3(ns("plotUnit"),"units",choices=c("in","cm","mm"),selected="in",width=70),
               if(count>0) numericInput3(ns("plotWidth"),"plotWidth",value=7,step=0.5,width=70),
               if(count>0) numericInput3(ns("plotHeight"),"plotHeight",value=5,step=0.5,width=70),
               if(count>0) radioButtons3(ns('plotformat'), 'Format As', c('PNG', 'SVG','PDF'),
                                         inline = TRUE,selected='PNG'),

               if(count==0) p("There is no saved data.")
               # if(count>0) hr(),
               # if(count>0) actionButton(ns("showPPTList"),"show/hide saved List")
               # ,
               # if(count>0) conditionalPanel("true==false",
               #                              checkboxInput(ns("showList"),"showList",value=FALSE)),
               # if(count>0) conditionalPanel(sprintf("input['%s']==true",ns("showList")),
               #                              hr(),
               #                              uiOutput(ns("PPTListUI2")))

          )
     })

     # observeEvent(input$showPPTList,{
     #     updateCheckboxInput(session,"showList",value=!input$showList)
     # })

     # output$PPTListUI2=renderUI({
     #
     #     input$showPPTList
     #
     #     ns<-session$ns
     #
     #     count= length(savedPPT$title)
     #
     #     mydf=data.frame(type=savedPPT$type,title=savedPPT$title,code=savedPPT$code,stringsAsFactors = FALSE)
     #
     #     for(i in 1:count){
     #         local({
     #             j<-i
     #             outputname=paste0("output",j*2-1)
     #             output[[outputname]]=renderPrint({
     #                 h4(mydf$title[j])
     #             })
     #         })
     #     }
     #     for(i in 1:count){
     #         local({
     #             j<-i
     #             outputname=paste0("output",j*2)
     #             if(savedPPT$type[j]=="table"){
     #                 output[[outputname]]=renderFlexTable({
     #                     mytable=eval(parse(text=mydf$code[j]))
     #                     mytable
     #                 })
     #             } else if(savedPPT$type[j]=="mytable"){
     #                 output[[outputname]]=renderFlexTable({
     #                     res=eval(parse(text=mydf$code[j]))
     #                     MyFTable=mytable2FTable(res,vanilla=TRUE)
     #                     MyFTable
     #                 })
     #             } else if(savedPPT$type[j]=="ggplot"){
     #                 output[[outputname]]=renderPlot({
     #                     p<-eval(parse(text=mydf$code[j]))
     #                     p
     #                 })
     #
     #             } else if(savedPPT$type[j]=="plot"){
     #                 output[[outputname]]=renderPlot({
     #                     p<-eval(parse(text=mydf$code[j]))
     #                     p
     #                 })
     #
     #             } else if(savedPPT$type[j]=="Rcode"){
     #                 output[[outputname]]=renderFlexTable({
     #                     result=Rcode2FlexTable(mydf$code[j])
     #                     result
     #
     #                 })
     #
     #             } else if(savedPPT$type[j]=="PNG"){
     #                 output[[outputname]]=renderImage({
     #                     myfunction<-eval(parse(text=mydf$code[j]))
     #                     png("temp.png",width=input$plotWidth,height=input$plotHeight,units=input$plotUnit,
     #                         res=input$plotRes,type="cairo")
     #                     myfunction
     #                     dev.off()
     #                     list(src = "temp.png",
     #                          contentType = 'image/png',
     #                          width = 400,
     #                          height = 300,
     #                          alt = "This is alternate text")
     #                 }, deleteFile = TRUE)
     #             }
     #         })
     #
     #     }
     #     output_list <- lapply(1:count, function(j) {
     #
     #         outputname=paste0("output",j*2)
     #         if(mydf$type[j] %in% c("table","mytable","Rcode")) tableOutput(ns(outputname))
     #         else if(mydf$type[j] %in% c("ggplot","plot")) plotOutput(ns(outputname))
     #         else if(mydf$type[j]=="PNG") imageOutput(ns(outputname))
     #
     #     })
     #     output_list2 <- lapply(1:count, function(j) {
     #
     #         outputname=paste0("output",j*2-1)
     #         htmlOutput(ns(outputname))
     #
     #     })
     #
     #     # Convert the list to a tagList - this is necessary for the list of items
     #     # to display properly.
     #     my_list=c(output_list,output_list2)
     #     for(i in 1:length(output_list)){
     #         my_list[[2*i-1]]=output_list2[[i]]
     #         my_list[[2*i]]=output_list[[i]]
     #     }
     #
     #     do.call(tagList, my_list)
     #
     #
     # })




     observeEvent(input$ResetPPT,{

          savedPPT$type=c()
          savedPPT$title=c()
          savedPPT$code=c()

          updateTextAreaInput(session,"preprocessing",value="")

     })

     observeEvent(input$loadSample,{

         savedPPT$type=rrtable::sampleData2$type
         savedPPT$title=rrtable::sampleData2$title
         savedPPT$code=rrtable::sampleData2$code

         updateTextAreaInput(session,"preprocessing",value="")

     })


     pptdf2=callModule(editableDT,"PPTxListTable",data=reactive(pptdf()))


     output$downloadPPTxHTML<- downloadHandler(
          filename = function() {
               paste('report', sep = '.','html')
          },

          content = function(file) {
               # src <- normalizePath('PPTxReport.Rmd')

               # temporarily switch to the temp dir, in case you do not have write
               # permission to the current working directory
               owd <- setwd(tempdir())
               on.exit(setwd(owd))

               data2HTML(pptdf2(),preprocessing=input$preprocessing,filename=file,echo=input$showCode)
          }
     )

     output$downloadPPTxPDF<- downloadHandler(
          filename = function() {
               paste('report', sep = '.','pdf')
          },

          content = function(file) {

               # temporarily switch to the temp dir, in case you do not have write
               # permission to the current working directory
               owd <- setwd(tempdir())
               on.exit(setwd(owd))

               data2pdf(pptdf2(),preprocessing=input$preprocessing,filename=file,echo=input$showCode)
          }
     )

     output$downloadPPTxList=downloadHandler(
          filename="report.pptx",
          content=function(file){

               # temporarily switch to the temp dir, in case you do not have write
               # permission to the current working directory
               owd <- setwd(tempdir())
               on.exit(setwd(owd))

               data=pptdf2()

               data2pptx(data,
                         preprocessing=input$preprocessing,
                         filename=file,width=input$width,height=input$height,units=input$units,res=input$res,echo=input$showCode)

          },
          contentType="application/vnd-ms-powerpoint"
     )



     output$downloadPPTxListWord=downloadHandler(
          filename="report.docx",
          content=function(file){

               owd <- setwd(tempdir())
               on.exit(setwd(owd))

               data=pptdf2()

               data2docx(data,
                         preprocessing=input$preprocessing,
                         filename=file,width=input$plotWidth,height=input$plotHeight,
                         units=input$plotUnit,res=input$plotRes,echo=input$showCode)


          },
          contentType="application/vnd-ms-word"
     )



     output$savePPTxList = downloadHandler(
          filename="PPTxList.csv",
          content=function(file){


               owd <- setwd(tempdir())
               on.exit(setwd(owd))
               df<-pptdf2()
               writeCSVComment(df,file=file,metadata=input$preprocessing)
          },
          contentType="text/csv"
     )

     output$downloadPlots = downloadHandler(
          filename="Plot.zip",
          content=function(file){

               # temporarily switch to the temp dir, in case you do not have write
               # permission to the current working directory
               owd <- setwd(tempdir())
               on.exit(setwd(owd))

               data=pptdf2()

               data2plotzip(data,filename=file,format=input$plotformat,width=input$plotWidth,
                            height=input$plotHeight,units=input$plotUnit,res=input$plotRes,start=0,
                            preprocessing=input$preprocessing)
          },
          contentType="application/zip"
     )

     pptdf3<-reactive({
          result<-NULL
          result<-pptdf2()
          if(input$preprocessing!="") {
               attr(result,"preprocessing")=input$preprocessing
          }
          result
     })

     return(pptdf3)
}












