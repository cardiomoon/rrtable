library(shiny)
library(rrtable)

ui<-fluidPage(
    h3("Demonstration of Asynchronous Progress Bar"),
    p("Press buttons many times..."),
    useAsyncProgressBar(),
    actionButton("Go","Go"),
    actionButton("Go1","Go1"),
    actionButton("Go2","Go2"),
    actionButton("Go5","Go5")
)

server=function(input,output,session){

    observeEvent(input$Go,{
       asyncProgressBar()
       data2HTML(sampleData2)
    })

    observeEvent(input$Go1,{
        asyncProgressBar(1)
        asyncProgressBar()
    })
    observeEvent(input$Go2,{
        asyncProgressBar()
    })
    observeEvent(input$Go5,{
        asyncProgressBar(second=5,message="Now rendering...")
    })


}


shinyApp(ui, server)
