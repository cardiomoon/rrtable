library(shiny)
library(rrtable)

ui<-fluidPage(
    timeProgressUI("timeProgress"),
    actionButton("Go","Go"),
    actionButton("Go1","Go1"),
    actionButton("Go2","Go2"),
    actionButton("Go5","Go5")
)

server=function(input,output,session){

    observeEvent(input$Go1,{
        data2HTML("sampleData2")
    })

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
