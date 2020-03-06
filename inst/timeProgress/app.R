library(shiny)
library(webrSub)

ui<-fluidPage(
    h3("Demonstration of Asynchronous Progress Bar"),
    p("Press buttons many times..."),
    useAsyncProgressBar(),
    actionButton("Go1","Go1"),
    actionButton("Go2","Go2"),
    actionButton("Go5","Go5")
)

server=function(input,output,session){

    observeEvent(input$Go,{
       data2HTML(sampleData2)
    })

    observeEvent(input$Go,{
    callModule(timeProgress,"timeProgress",input$second)
    })

    observeEvent(input$Go1,{
        asyncProgressBar(1)
    })
    observeEvent(input$Go2,{
        asyncProgressBar()
    })
    observeEvent(input$Go5,{
        asyncProgressBar(second=5,message="Now rendering...")
    })


}


shinyApp(ui, server)
