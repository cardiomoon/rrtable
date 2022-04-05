## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE ,message=FALSE,warning=FALSE,comment=NA,eval=FALSE,
          fig.width=9,fig.asp=0.618,fig.align='center',out.width='70%')

## ----echo= TRUE ,message=FALSE------------------------------------------------
#  require(rrtable)
#  require(ggplot2)

## -----------------------------------------------------------------------------
#  x=ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()

## -----------------------------------------------------------------------------
#  plot2pptx(x)

## -----------------------------------------------------------------------------
#  x="ggplot(mtcars,aes(x=wt,y=mpg))+geom_point()"
#  plot2pptx(x,echo=TRUE,title="A ggplot")

## ----eval=FALSE---------------------------------------------------------------
#  plot2pptx("plot(iris)",echo=TRUE,append=TRUE)

## -----------------------------------------------------------------------------
#  table2pptx("head(iris)", echo=TRUE,append=TRUE)

## -----------------------------------------------------------------------------
#  fit=lm(mpg~ wt*hp, data=mtcars)
#  Rcode2pptx("summary(fit)",append=TRUE)

## -----------------------------------------------------------------------------
#  fit2=aov(yield ~ block + N * P + K, data = npk)
#  table2pptx(fit2,title="ANOVA result",append=TRUE,vanilla=TRUE)

## -----------------------------------------------------------------------------
#  table2pptx("aov(yield ~ block + N * P + K, data = npk)",
#             title="ANOVA result",echo=TRUE,append=TRUE,vanilla=TRUE)

## -----------------------------------------------------------------------------
#  x=c("plot(iris)","ggplot(mtcars,aes(x=hp,y=mpg))+geom_point()")
#  plot2office(x,title="2 plots",parallel=TRUE,echo=TRUE,append=TRUE)

## -----------------------------------------------------------------------------
#  shiny::runApp(system.file('pptxList',package='rrtable'))

