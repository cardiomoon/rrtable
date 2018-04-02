library(shiny)
library(rrtable)
library(ggplot2)

ui=fluidPage(
    pptxListInput("pptxlist")
)
server=function(input,output,session){

     mydf<-callModule(pptxList,"pptxlist")
}
shinyApp(ui,server)
