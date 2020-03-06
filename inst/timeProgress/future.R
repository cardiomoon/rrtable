library(shiny)
library(ipc)
library(future)
library(promises)
library(rrtable)
plan(multiprocess)


#'UI of timeProgress Bar
#'@param id id
#'@export
APUI=function(id){
    ns<-NS(id)

    uiOutput(ns("AP"))
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
AP<- function(input, output,session, second=2, message=NULL,interval=100) {


    ns<-NS(session$id)

    if(is.null(message)) message=paste("Wait for",second,"second(s)...")

    # A reactive value to hold output
    result_val <- reactiveVal()

    # Handle button click
    observe({
        result_val(NULL)

        # Create a progress bar
        progress <- AsyncProgress$new(message="Complex analysis")
        future({
            for(i in 1:(second*10)){
                Sys.sleep(0.1)
                progress$inc(1/(second*10)) # Increment progress bar
            }
            progress$close() # Close the progress bar
            data.frame(result="Insightful result")
        }) %...>% result_val  # Assign result of future to result_val

        # Return something other than the future so we don't block the UI
        NULL
    })

}

ui<-fluidPage(
    useAsyncProgressBar(),
    APUI("AP"),
    actionButton("Go","Go"),
    actionButton("Go1","Go1"),
    actionButton("Go2","Go2"),
    actionButton("Go5","Go5")
)

server=function(input,output,session){


    observeEvent(input$Go,{
        callModule(AP,"AP")
        callModule(AP,"AP")

    })


    observeEvent(input$Go1,{
        callModule(AP,"AP",1)
    })

    observeEvent(input$Go2,{
        callModule(AP,"AP",2)
    })

    observeEvent(input$Go5,{
        callModule(AP,"AP",5)
    })


}
# Run the application
shinyApp(ui = ui, server = server)

