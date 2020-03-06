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


#' Side by side pickerInput
#' @param ... Further arguments to be passed to pickerInput
#' @importFrom shinyWidgets pickerInput
#' @export
pickerInput3=function (...)
{
        div(style = "display:inline-block;", pickerInput(...))
}

#' Server function of pptxList shiny module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param data A data object
#' @param preprocessing A character string of R code
#' @importFrom shiny reactiveValues updateTextAreaInput reactive h4 fileInput actionButton downloadButton hr p callModule
#' @importFrom shiny downloadHandler tagList conditionalPanel uiOutput observe observeEvent column fluidRow renderUI h3 br
#' @importFrom shiny htmlOutput imageOutput plotOutput renderImage renderPlot renderPrint tableOutput updateCheckboxInput verbatimTextOutput
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
             temp=preprocessing
          if(length(temp)!=0){
               updateTextAreaInput(session,"preprocessing",value=preprocessing())
          }
     })

     pptdf=reactive({

          input$pptfile


          df<-data.frame(type=savedPPT$type,
                         title=savedPPT$title,
                         code=savedPPT$code,
                         stringsAsFactors = FALSE)

          df
     })

     observeEvent(input$pptfile,{
          if(!is.null(input$pptfile)){

               mypptlist<-readr::read_csv(input$pptfile$datapath,comment="#")
               mypptlist[is.na(mypptlist)]=""
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
                           fileInput(ns("pptfile"),NULL)  #"Upload PPTList(*.csv)"
                    ),
                    column(3,
                           h4("Load sampleData "),
                           actionButton(ns("loadSample"),"load sampleData")
                    )
               ),

               if(count>0) checkboxInput(ns("showCode"),"show Code in Output File"),
               if(count>0) hr(),
               if(count>0) h3("With All Slides"),
               if(count>0) downloadButton(ns("savePPTxList"),"download as csv"),
               if(count>0) downloadButton(ns("downloadPPTxHTML"),"download as HTML"),
               if(count>0) downloadButton(ns("downloadPPTxPDF"),"download as PDF"),
               if(count>0) downloadButton(ns("downloadPPTxList"),"download as PPTx"),
               if(count>0) downloadButton(ns("downloadPPTxListWord"),"download as Word"),
               if(count>0) br(),
               if(count>0) br(),
               if(count>0) downloadButton(ns("downloadPlots"),"download Plots"),
               if(count>0) numericInput3(ns("plotRes"),"Resolution",value=300,step=1,width=80),
               if(count>0) pickerInput3(ns("plotUnit"),"units",choices=c("in","cm","mm"),selected="in",width="70px"),
               if(count>0) numericInput3(ns("plotWidth"),"plotWidth",value=7,step=0.5,width=70),
               if(count>0) numericInput3(ns("plotHeight"),"plotHeight",value=5,step=0.5,width=70),
               if(count>0) radioButtons3(ns('plotformat'), 'Format As', c('PNG', 'SVG','PDF'),
                                         inline = TRUE,selected='PNG'),
               if(count>0) hr(),
               if(count>0) uiOutput(ns("choosePPT")),
               if(count>0) hr()



          )
     })

     output$choosePPT=renderUI({
             ns <- session$ns
             tagList(
                     fluidRow(
                     column(6,
                            h3("Select Slides"),
                            chooser2UI(ns("chooser"))
                            # ,verbatimTextOutput(ns("chooserText"))
                ),
                column(6,
                       h3("With Selected"),
                       actionButton(ns("delsel"),"Delete Selected"),
                       actionButton(ns("delunsel"),"Delete Unselected"),
                       hr(),
                       downloadButton(ns("downloadPPTxHTML2"),"download as HTML"),
                       downloadButton(ns("downloadPPTxPDF2"),"download as PDF"),
                       br(),
                       br(),
                       downloadButton(ns("downloadPPTxList2"),"download as PPTx"),
                       downloadButton(ns("downloadPPTxListWord2"),"download as Word"),
                       br(),
                       br(),
                       downloadButton(ns("downloadPlots2"),"download Plots")
                       )
                     )
             )
     })

     chooseResult=callModule(chooser2,"chooser",leftChoices=reactive(1:nrow(pptdf())))

     # output$chooserText=renderPrint({
     #         str(chooseResult())
     # })

     observeEvent(input$delsel,{
             selected=chooseResult()$right
             selected=as.numeric(selected)

             savedPPT$type=savedPPT$type[-selected]
             savedPPT$title=savedPPT$title[-selected]
             savedPPT$code=savedPPT$code[-selected]

     })

     observeEvent(input$delunsel,{
             selected=chooseResult()$right
             selected=as.numeric(selected)

             savedPPT$type=savedPPT$type[selected]
             savedPPT$title=savedPPT$title[selected]
             savedPPT$code=savedPPT$code[selected]

     })


     observeEvent(input$deleteRow,{
         selected=input$pptchooser$right
         selected=as.numeric(selected)

         savedPPT$type=savedPPT$type[-selected]
         savedPPT$title=savedPPT$title[-selected]
         savedPPT$code=savedPPT$code[-selected]
     })



     loadSample=function(){
             savedPPT$type=rrtable::sampleData2$type
             savedPPT$title=rrtable::sampleData2$title
             savedPPT$code=rrtable::sampleData2$code

             updateTextAreaInput(session,"preprocessing",value="")
     }

     observeEvent(input$loadSample,{

         loadSample()

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

               data2docx2(data,
                         preprocessing=input$preprocessing,
                         filename=file,echo=input$showCode)


          },
          contentType="application/vnd-ms-word"
     )

     output$downloadPPTxHTML2<- downloadHandler(
             filename = function() {
                     paste('report', sep = '.','html')
             },

             content = function(file) {
                     # src <- normalizePath('PPTxReport.Rmd')

                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))

                     selected=chooseResult()$right
                     selected=as.numeric(selected)
                     data=pptdf2()[selected,]

                     data2HTML(data,preprocessing=input$preprocessing,filename=file,echo=input$showCode)
             }
     )

     output$downloadPPTxPDF2<- downloadHandler(
             filename = function() {
                     paste('report', sep = '.','pdf')
             },

             content = function(file) {

                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))

                     selected=chooseResult()$right
                     selected=as.numeric(selected)
                     data=pptdf2()[selected,]

                     data2pdf(data,preprocessing=input$preprocessing,filename=file,echo=input$showCode)
             }
     )

     output$downloadPPTxList2=downloadHandler(
             filename="report.pptx",
             content=function(file){

                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))

                     selected=chooseResult()$right
                     selected=as.numeric(selected)
                     data=pptdf2()[selected,]

                     data2pptx(data,
                               preprocessing=input$preprocessing,
                               filename=file,width=input$width,height=input$height,units=input$units,res=input$res,echo=input$showCode)

             },
             contentType="application/vnd-ms-powerpoint"
     )


     output$downloadPPTxListWord2=downloadHandler(
             filename="report.docx",
             content=function(file){

                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))

                     selected=chooseResult()$right
                     selected=as.numeric(selected)
                     data=pptdf2()[selected,]

                     data2docx2(data,
                                preprocessing=input$preprocessing,
                                filename=file,echo=input$showCode)


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

     output$downloadPlots2 = downloadHandler(
             filename="Plot.zip",
             content=function(file){

                     # temporarily switch to the temp dir, in case you do not have write
                     # permission to the current working directory
                     owd <- setwd(tempdir())
                     on.exit(setwd(owd))

                     selected=chooseResult()$right
                     selected=as.numeric(selected)
                     data=pptdf2()[selected,]

                     data2plotzip(data,filename=file,format=input$plotformat,width=input$plotWidth,
                                  height=input$plotHeight,units=input$plotUnit,res=input$plotRes,start=0,
                                  preprocessing=input$preprocessing)
             },
             contentType="application/zip"
     )

     pptdf3<-reactive({


          result<-NULL
          try(result<-pptdf2())
          if(input$preprocessing!="") {
               attr(result,"preprocessing")=input$preprocessing
          }
          # str(pptdf2())
          result
     })

     return(pptdf3)
}












