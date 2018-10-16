library(shiny)
library(rrtable)
library(ggplot2)
library(webr)
library(moonBook)

ui=fluidPage(
    pptxListInput("pptxlist")
)
server=function(input,output,session){

     mydf<-callModule(pptxList,"pptxlist")

}
shinyApp(ui,server)
