###### Library ######
library(shiny)
library(shinyjs)
library(shinydashboard)
library(raster)
library(DT)
library(shinyWidgets)
library(keys)
library(shinyalert)
library(sf)
library(exactextractr)
library(lwgeom)
library(plyr)
library(here)
###### Variables ######
options(stringsAsFactors = F)
rstFiles<-list.files(here::here('R','Shiny_frontEnd','demo','PIC','rst'),full.names = T)
rstComic<-lapply(rstFiles,raster)
rstStack<-stack(rstComic)
rst<-rstStack
rst<-as.list(rstStack)
nmsRst<-names(rstStack)
names(rst)<-nmsRst
plc<-colorRampPalette(c('black','white'))
plcn<-255
pWidth = 500
pHeight<-pWidth*((rst[[1]]@extent[4]-rst[[1]]@extent[3])/(rst[[1]]@extent[2]-rst[[1]]@extent[1]))
hotKey<-c('p','d','n','g','shift+v','enter','shift+c','t')
NAcol<-'blue'
options(shiny.maxRequestSize=500*1024^2)    

