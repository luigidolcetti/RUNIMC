shinyServiceEnv<-new.env()


shinyServiceEnv$rstPath<-list.files(system.file("shiny-bits\\ShinyRUNIMC\\demo\\PIC\\rst",
                                package = "RUNIMC"),
                    pattern = '*.nc',
                    full.names = T)
shinyServiceEnv$rstPic<-sapply(shinyServiceEnv$rstPath,raster::raster,simplify = F,USE.NAMES = F)
names(shinyServiceEnv$rstPic)<-paste0('lyr',1:length(shinyServiceEnv$rstPic))
shinyServiceEnv$rstStack<-RUNIMC:::IMC_stack(x = shinyServiceEnv$rstPic,
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
shinyServiceEnv$nmsRst<-names(shinyServiceEnv$rstStack)
shinyServiceEnv$plc<-colorRampPalette(c('black','white'))
shinyServiceEnv$plcn<-255
shinyServiceEnv$pWidth = 500
shinyServiceEnv$pHeight<-shinyServiceEnv$pWidth*((shinyServiceEnv$rstStack[[1]]@extent[4]-shinyServiceEnv$rstStack[[1]]@extent[3])/(shinyServiceEnv$rstStack[[1]]@extent[2]-shinyServiceEnv$rstStack[[1]]@extent[1]))
shinyServiceEnv$hotKey<-c('p','d','n','g','shift+v','enter','shift+c','t')
shinyServiceEnv$NAcol<-'blue'
options(shiny.maxRequestSize=1000*1024^2)

