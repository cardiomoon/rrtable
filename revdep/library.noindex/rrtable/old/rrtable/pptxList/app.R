library(shiny)
library(rrtable)
library(ggplot2)
library(moonBook)
library(shinybusy)

ui=fluidPage(
    pptxListInput("pptxlist")
)
server=function(input,output,session){

     mydf<-callModule(pptxList,"pptxlist")

}
shinyApp(ui,server)
