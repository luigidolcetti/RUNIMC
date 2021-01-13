
rstStack<-IMCstackOpen(system.file("shiny-bits\\ShinyRUNIMC\\demo.stk", package = "RUNIMC"))
rstStack@uid<-"demo"
nmsRst<-names(rstStack)
plc<-colorRampPalette(c('black','white'))
plcn<-255
pWidth = 500
pHeight<-pWidth*((rstStack[[1]]@extent[4]-rstStack[[1]]@extent[3])/(rstStack[[1]]@extent[2]-rstStack[[1]]@extent[1]))
hotKey<-c('p','d','n','g','shift+v','enter','shift+c','t')
NAcol<-'blue'
options(shiny.maxRequestSize=1000*1024^2)

