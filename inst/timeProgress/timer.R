library(shiny)
ui <- fluidPage(
    numericInput("n", "n",value=5),
    numericInput("count", "count",value=0),
    actionButton("Go","Go"),
    sliderInput("no1","no1",value=100,min=1,max=1000),
    plotOutput("plot")

)

server <- function(input, output,session) {



    output$plot <- renderPlot({

        hist(rnorm(input$no1))
    })

    RV=reactiveValues()
    RV$progress=NULL


     observeEvent(input$Go,{
        updateNumericInput(session,"count",value=0)
        RV$progress <- shiny::Progress$new()
        RV$progress$set(message = paste("Timer of",input$n,"seconds"), value = 0)

    })

    observe({
        req(input$Go)

        invalidateLater(100)

        no<-isolate(input$count)
        if(no<=input$n*10){
             RV$progress$inc(1/(input$n*10), detail = paste(no))
             updateNumericInput(session,"count",value=no+1)
             if(no==input$n*10)  RV$progress$close()

        }

    })

}

shinyApp(ui, server)
