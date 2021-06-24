library(jpeg)
library(bmp)
library(imager)
jpgFiles<-list.files("C:/Users/k1343421/Documents/IMC/R_IMC2/IMC_PRJ_2/Shiny_frontEnd/demo/PIC/jpg",full.names = T)
jpgpic<-lapply(jpgFiles,load.image)
jpgpic<-lapply(jpgpic,grayscale)
jpgpic<-lapply(jpgpic,as.matrix)

rstpic<-lapply(jpgpic,
               function(x){
                 raster((x),
                        xmn=0.5,
                        xmx=ncol(jpgpic[[1]])+0.5,
                        ymn=0.5,
                        ymx=nrow(jpgpic[[1]])+0.5)})
names(rstpic)<-paste0("layer-",1:5)
rstpic<-lapply(names(jpgpic),function(x){
  writeRaster(rstpic[[x]],
              paste0("C:/Users/k1343421/Documents/IMC/R_IMC2/IMC_PRJ_2/Shiny_frontEnd/demo/PIC/rst/",x),format='CDF',overwrite=T)})
rstFiles<-list.files("C:/Users/k1343421/Documents/IMC/R_IMC2/IMC_PRJ_2/Shiny_frontEnd/demo/PIC/rst",full.names = T)
stkpic<-stack(rstFiles)
stkpic<-as(stkpic,'IMC_RasterStack')
IMCstackSave(stkpic,"C:/Users/k1343421/Documents/IMC/R_IMC2/IMC_PRJ_2/Shiny_frontEnd/demo.stk"  )
