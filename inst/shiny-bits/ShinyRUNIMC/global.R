
rstPath<-list.files(system.file("shiny-bits\\ShinyRUNIMC\\demo\\PIC\\rst",
                                package = "RUNIMC"),
                    pattern = '*.nc',
                    full.names = T)
rstPic<-sapply(rstPath,raster::raster,simplify = F,USE.NAMES = F)
names(rstPic)<-paste0('lyr',1:length(rstPic))
rstStack<-RUNIMC:::IMC_stack(x = rstPic,
                             uid = "demo",
                             IMC_text_file = "demo",
                             study = NULL,
                             sample = NULL,
                             replicate = NULL,
                             ROI = NULL,
                             bioGroup = NULL,
                             channels = data.frame(0))
# rstStack<-IMCstackOpen(system.file("shiny-bits\\ShinyRUNIMC\\demo.stk", package = "RUNIMC"))
# rstStack@uid<-"demo"
nmsRst<-names(rstStack)
plc<-colorRampPalette(c('black','white'))
plcn<-255
pWidth = 500
pHeight<-pWidth*((rstStack[[1]]@extent[4]-rstStack[[1]]@extent[3])/(rstStack[[1]]@extent[2]-rstStack[[1]]@extent[1]))
hotKey<-c('p','d','n','g','shift+v','enter','shift+c','t')
NAcol<-'blue'
options(shiny.maxRequestSize=1000*1024^2)

