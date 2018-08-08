library(shiny)
library(rrtable)
library(ggplot2)

ui=fluidPage(
    pptxListInput("pptxlist"),
    tableOutput("table1")
)
server=function(input,output,session){

     mydf<-callModule(pptxList,"pptxlist")

     output$table1=renderTable(mydf())


}
shinyApp(ui,server)
