library(shiny)
library(rrtable)

#'Asynchronous progress bar UI function
#'@export
useAsyncProgressBar=function(){
    timeProgressUI("timeProgress")
}

#' Asynchronous progress bar sever function
#' @param second Numeric The timer of progress bar in seconds
#' @param message character Message of progress bar
#' @param interval numeric milliseconds
#' @importFrom shiny callModule
#' @export
asyncProgressBar=function(second=2,message=NULL,interval=100){
    callModule(timeProgress,"timeProgress",second=second,message=message,interval=interval)
}



#'UI of timeProgress Bar
#'@param id id
#'@export
timeProgressUI=function(id){
    ns<-NS(id)
}


#'Server function of asyncProgressBar Module
#'
#' @param input input
#' @param output output
#' @param session session
#' @param second Numeric The timer of progress bar in seconds
#' @param message character Message of progress bar
#' @param interval numeric milliseconds
#' @importFrom shiny reactiveValues  invalidateLater
#' @export
timeProgress <- function(input, output, session, second=2,message=NULL,interval=100) {

    ns<-NS(session$id)

    if(is.null(message)) message=paste("Wait for",second,"second(s)...")

    RV=reactiveValues()
    RV$progress=NULL
    RV$progress <- shiny::Progress$new()
    RV$progress$set(message = message, value = 0)


    i=0
    observe({

        invalidateLater(interval)

        if(i<=second*1000/interval){
            RV$progress$inc(1/(second*1000/interval),
                            detail = paste((i*100)/(second*1000/interval),"%"))
            if(i==second*1000/interval)  RV$progress$close()
            i<<-i+1
        }

    })

}

ui<-fluidPage(
    timeProgressUI("timeProgress"),
    actionButton("Go1","Go1"),
    actionButton("Go2","Go2"),
    actionButton("Go5","Go5")
)

server=function(input,output,session){

    observeEvent(input$Go1,{
        callModule(timeProgress,"timeProgress",1)
    })
    observeEvent(input$Go2,{
        callModule(timeProgress,"timeProgress",2)
    })
    observeEvent(input$Go5,{
        callModule(timeProgress,"timeProgress",second=5,message="Now rendering...")
    })


}


shinyApp(ui, server)
