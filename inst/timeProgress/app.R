library(shiny)
library(rrtable)

#'UI of timeProgress Bar
timeProgressUI=function(id){
   ns<-NS(id)

   uiOutput(ns("timer"))
}


#'Server function of timeProgress Module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param trigger reactive value of trigger
#' @param second The timer of progress bar
timeProgress <- function(input, output, session, trigger,second) {

    ns<-NS(session$id)

    output$timer=renderUI({

              req(trigger())

              isolate({
                  progress <- shiny::Progress$new()
                  on.exit(progress$close())
                  progress$set(message = "Making plot", value = 0)
                  n=second*10

                  for(i in 1:n) {
                      progress$inc(1/n, detail = paste("Doing part", i*2,"%"))
                      Sys.sleep(0.1)
                  }
              })

    })
}

ui<-fluidPage(
    numericInput("second","second",value=5),
    actionButton("Go","Go"),
    timeProgressUI("timeProgress"),
    actionButton("Go1","Go1"),
    actionButton("Go2","Go2")
)

server=function(input,output,session){

    observeEvent(input$Go,{
       data2HTML(sampleData2)
    })
    callModule(timeProgress,"timeProgress",reactive(input$Go),input$second)
    callModule(timeProgress,"timeProgress",reactive(input$Go1),1)
    callModule(timeProgress,"timeProgress",reactive(input$Go2),2)
}


shinyApp(ui, server)
