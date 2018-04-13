title="Plot"
code="plot(iris)"
eval=FALSE
codeft=Rcode2flextable(code,eval=eval,format="pptx")
plotstring="plot(iris)"
temp=paste0("ph_with_vg_at(mydoc,code=",plotstring,",left=1,top=2,width=8,height=5)")

pos=1.5
require(officer)
require(flextable)
require(rvg)
require(rrtable)
require(magrittr)
mydoc<-read_pptx() %>%  add_slide(layout = "Title Only", master = "Office Theme") %>%
    ph_with_text(type="title",str=title) %>%
    add_plot(plotstring)

mydoc=eval(parse(text=temp))
print(mydoc,target=paste0(getwd(),"/","report.pptx"))


mydoc<-read_pptx() %>%  add_text(title=title,text="",code=code,echo=TRUE,eval=TRUE) %>%
    add_plot(plotstring)
mydoc=eval(parse(text=temp))
print(mydoc,target=paste0(getwd(),"/","report.pptx"))

sampleData2[5,]
data2pptx(sampleData2[5,],echo=TRUE)
data2pptx(sampleData2[5,])
data2pptx(sampleData2,echo=TRUE)
data2pptx(sampleData2[5,],echo=TRUE)
data2pptx(sampleData2[4:5,],echo=TRUE)
