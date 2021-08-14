#'Some nice thing
#'
#'
#' @export
monkeyForest<-function(fn_rst = NULL,
                       fn_layers = NULL,
                       fn_newLayerName='label',
                       fn_undeterminedLabel='undetermined',
                       fn_Ptreshold=0,
                       fn_forest = NULL,
                       fn_filePath=NULL){

  newFilePath<-checkDir(file.path(fn_filePath,'rasters'),fn_rst@IMC_text_file)
  winnerSelection<-function(x,trsh=fn_Ptreshold,na.rm){
    winnerClass<-which(x==max(x))
    if (length(winnerClass)==0) return(length(x)+1)
    if (length(winnerClass)>1) return(length(x)+1)
    if (x[winnerClass[1]]>trsh) return(winnerClass[1]) else return(length(x)+1)
  }
  rfClasses<-fn_forest$classes
  rfClassNumber<-length(rfClasses)
  newStack <-raster::predict(fn_rst,fn_forest,type='prob',progress='text',index=seq_along(rfClasses))
  newRaster<-raster::stackApply(x = newStack,
                                indices = rep(1,rfClassNumber),
                                fun=winnerSelection)

  names(newRaster)<-fn_newLayerName

  trainingLabels<-c(rfClasses,fn_undeterminedLabel)

  rat<-data.frame(ID=seq_along(trainingLabels),label=trainingLabels)

  levels(newRaster)[[1]]<-rat

  newRstList<-raster::as.list(newStack)
  names(newRstList)<-names(newStack)
  newRstList[[fn_newLayerName]]<-newRaster

  newRstList<-sapply(names(newRstList), function(nms){
    rstfilePath<-file.path(newFilePath,paste0(nms,'.grd'))
    newRaster<-raster::writeRaster(x = newRstList[[nms]],
                                   filename = rstfilePath,
                                   overwrite=T,
                                   format='raster')
    return(newRaster)
  })

  rstrStk<-IMC_stack(x = newRstList,
                     uid = fn_rst@uid,
                     IMC_text_file = fn_rst@IMC_text_file,
                     study = fn_rst@study,
                     sample = fn_rst@sample,
                     replicate = fn_rst@replicate,
                     ROI = fn_rst@ROI,
                     bioGroup = fn_rst@bioGroup,
                     channels = fn_rst@channels)

  rstrStk<-IMCstackSave(rstrStk,file.path(fn_filePath,'rasterStacks',paste0(fn_rst@IMC_text_file,'.stk')))

  return(rstrStk)
}
