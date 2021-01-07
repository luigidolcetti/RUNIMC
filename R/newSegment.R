topoMap<-function(fn_classification=NULL,
                  fn_classificationLyr=NULL,
                  fn_label=NULL,
                  fn_area=NULL,
                  fn_prefix='topoMap_',
                  fn_raster=NULL,
                  fn_derivedRaster=NULL,
                  fn_features=NULL,
                  fn_maxClumps=10,
                  fn_clumpDirection=8,
                  fn_mtry=length(fn_features)/3,
                  fn_ntree=100,
                  fn_trace=10,
                  fn_filePath=NULL){


  if (is.null(fn_classification)) stop(RUNIMC:::mError('no classification specified'))
  if (is.null(fn_classificationLyr)) stop(RUNIMC:::mError('no classification layer specified'))
  if (is.null(fn_label)) stop(RUNIMC:::mError('no classification label specified'))
  if (is.null(fn_area)) stop(RUNIMC:::mError('no area limits specified'))
  if (class(fn_area)=='numeric' & length(fn_area)==2){
    fn_area<-data.frame(label=fn_label,
                        area=matrix(rep(fn_area,length(fn_label)),
                                    ncol = 2,
                                    byrow = T))
  }
  if (!is.null(fn_filePath)){
    TMfilePath<-RUNIMC:::checkDir(parentalFolder = fn_filePath,childFolder = 'topoMap')
    filePathRaster<-RUNIMC:::checkDir(parentalFolder = TMfilePath,childFolder = 'rasters')
    filePathRasterStack<-RUNIMC:::checkDir(parentalFolder = TMfilePath,childFolder = 'rasterStacks')
  }

  smpCl<-names(fn_classification)
  smpRs<-names(fn_raster)

  # if (smpCl!=smpRs) stop(RUNIMC:::mError('sample numbers differ'))

  # if (!is.null(fn_derivedRaster)){
  #   smpDr<-names(fn_derivedRaster)
  #   if (smpCl!=smpDr) stop(RUNIMC:::mError('sample numbers differ'))
  # }

  superRaster<-sapply(smpCl,function(smp){
    if (!is.null(fn_derivedRaster)){
      superRaster<-raster::stack(fn_raster[[smp]],fn_derivedRaster[[smp]])
    } else {
      superRaster<-fn_raster[[smp]]
    }
    return(superRaster)
  },USE.NAMES = T,simplify = F)

  classMask<-sapply(smpCl,function(smp){
    out<-sapply(fn_label,function(lbl){

      lvls<-raster::levels(fn_classification[[smp]][[fn_classificationLyr]])[[1]]
      rowLabels<-which(grepl(lbl,lvls$label))
      IDlabels<-lvls$ID[rowLabels]
      labelMask<-lapply(IDlabels,function(x){
        fn_classification[[smp]][[fn_classificationLyr]]==x
      })
      labelMask<-raster::stack(labelMask)
      labelMask<-raster::calc(labelMask,sum)
      return(labelMask)
    },USE.NAMES = T,simplify = F)
    return(out)
  },USE.NAMES = T,simplify = F)



  tableOut<-sapply(smpCl,function(smp){


    labelOut<-sapply(fn_label,function(lbl){

      cat(paste0('Extracting...:::',lbl,'::: for sample ',smp,'\n'))

      areaLimits<-fn_area[fn_area[,1]==lbl,]
      clmps<-raster::clump(x=classMask[[smp]][[lbl]],directions=fn_clumpDirection,gaps=F)
      freqClmps<-raster::freq(clmps)
      freqClmps<-freqClmps[freqClmps[,'count']>min(areaLimits[1,2]) & freqClmps[,'count']<max(areaLimits[1,2]),]
      DFraster<-raster::as.data.frame(clmps,xy=T)
      DFraster<-na.omit(DFraster)
      if (fn_maxClumps<nrow(freqClmps)) freqClmps<-freqClmps[sample(1:nrow(freqClmps),fn_maxClumps),]
      clmpsMatrix<-pbapply::pblapply(freqClmps[,'value'],function(x){
        DFclpm<-DFraster[DFraster$clumps==x,]
        xMean<-mean(DFclpm$x)
        yMean<-mean(DFclpm$y)
        pD<-unname(raster::pointDistance(DFclpm[c('x','y')],c(xMean,yMean),lonlat = F))
        cellVector<-raster::cellFromXY(superRaster[[smp]],DFclpm[c('x','y')])
        valueMatrix<-raster::extract(x=superRaster[[smp]],y=DFclpm[c('x','y')],cellnumbers=T)
        DFclpm<-cbind.data.frame(DFclpm,pD,valueMatrix)
      })
      clmpsMatrix<-do.call(rbind.data.frame,clmpsMatrix)
    },USE.NAMES = T,simplify = F)


  },USE.NAMES = T,simplify = F)



  tableOut<-sapply(fn_label,function(lbl){
    out<-sapply(smpCl,function(smp){
      tableOut[[smp]][[lbl]]
    },USE.NAMES = F,simplify = F)
    do.call(rbind.data.frame,out)
  },USE.NAMES = T,simplify = F)



  rf<-sapply(fn_label,function(lbl){
    cat('Random Forest...:::',lbl,':::\n')
    rf<-randomForest::randomForest(x=tableOut[[lbl]][,fn_features],
                                   y=tableOut[[lbl]][,'pD'],
                                   mtry=fn_mtry,
                                   ntree=fn_ntree,
                                   do.trace=fn_trace)
    return(rf)
  },USE.NAMES = T,simplify = F)


  classOut<-sapply(smpCl,function(smp){
    if (!is.null(fn_filePath)){
      dirTXT<-fn_classification[[smp]]@IMC_text_file
      filePathRasterTXT<-RUNIMC:::checkDir(parentalFolder = filePathRaster,dirTXT)
    }
    labelOut<-sapply(fn_label,function(lbl){

      cat(paste0('predict raster for :::',lbl,'::: in ',smp,'\n'))
      maskedRaster<-raster::mask(x=superRaster[[smp]],
                                 mask = classMask[[smp]][[lbl]],
                                 maskvalue=1,
                                 inverse=T)

      outRst<-raster::predict(maskedRaster,
                              rf[[lbl]],
                              na.rm=T,
                              progress='text')
      names(outRst)<-lbl
      if (!is.null(fn_filePath)){
        fileObjective<-file.path(filePathRasterTXT,paste0(lbl,'.nc'))
        raster::writeRaster(x = outRst,
                            filename = fileObjective,
                            overwrite=T,
                            format='CDF')
        outRst<-raster::raster(fileObjective)
      }
      return(outRst)
    },USE.NAMES = T,simplify = F)
    # outStk<-raster::stack(labelOut)
    outStk<-RUNIMC:::IMC_stack(x = labelOut,
                               uid = fn_classification[[smp]]@uid,
                               IMC_text_file = fn_classification[[smp]]@IMC_text_file,
                               study = fn_classification[[smp]]@study,
                               sample = fn_classification[[smp]]@sample,
                               replicate = fn_classification[[smp]]@replicate,
                               ROI = fn_classification[[smp]]@ROI,
                               bioGroup = fn_classification[[smp]]@bioGroup,
                               channels = fn_classification[[smp]]@channels)
    if (!is.null(fn_filePath)){
      IMCstackSave(outStk,file.path(filePathRasterStack,paste0(fn_classification[[smp]]@IMC_text_file,'.stk')))
    }
    return(outStk)
  },USE.NAMES = T,simplify = F)
  return(classOut)
}
