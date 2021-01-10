#'Some BullShit
#'
#'
#' @export
monkeyForest<-function(fn_rst,
                       fn_layers,
                       fn_newLayerName='label',
                       fn_undeterminedLabel='undetermined',
                       fn_Ptreshold=NULL,
                       fn_colN,
                       fn_rowN,
                       fn_forest,
                       fn_filePath=NULL,
                       fn_append=T){


  testMatrix<-raster::getValues(fn_rst)[,fn_layers]

  prediction_for_table <-(predict(fn_forest,testMatrix,type='prob'))

  Nundt<-ncol(prediction_for_table)+1

  synthLabel<-unlist(apply(prediction_for_table,1,function(x){
    winLabel<-which(x==max(x))
    if (length(winLabel)!=1) winLabel<-Nundt
    if (!is.null(fn_Ptreshold) & winLabel!=Nundt){
      if (x[winLabel]<fn_Ptreshold) winLabel<-Nundt
    }
    return(winLabel)
  }),recursive = T,use.names = F)

  oldNames<-colnames(prediction_for_table)
  trainingLabels<-c(oldNames,fn_undeterminedLabel)

  prediction_for_table<-cbind(prediction_for_table,synthLabel)

  colnames(prediction_for_table)<-c(oldNames,fn_newLayerName)

  rat<-data.frame(ID=1:length(trainingLabels),label=trainingLabels)

  # prediction_for_table_numeric<-unname(sapply(prediction_for_table,function(x){
  #   rat$ID[rat$label==x]}))

  newFilePath<-checkDir(file.path(fn_filePath,'rasters'),fn_rst@IMC_text_file)

  newRstList<-sapply(colnames(prediction_for_table),function(Xcol){


    prediction_for_table_numeric<-matrix(prediction_for_table[,Xcol],
                                         ncol = fn_colN,
                                         nrow = fn_rowN,
                                         byrow = T)

    newRaster<-raster::raster(prediction_for_table_numeric,
                              xmn=0,
                              xmx=ncol(prediction_for_table_numeric),
                              ymn=0,
                              ymx=nrow(prediction_for_table_numeric),
                              crs=sp::CRS(as.character(NA)))

    names(newRaster)<-Xcol

    if (names(newRaster)==fn_newLayerName){

      levels(newRaster)[[1]]<-rat
    }

    if (!is.null(fn_filePath)){
      rstfilePath<-file.path(newFilePath,paste0(Xcol,'.grd'))
      raster::writeRaster(x = newRaster,
                          filename = rstfilePath,
                          overwrite=T,
                          format='raster')
      newRaster<-raster(rstfilePath)
    }


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
