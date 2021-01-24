#'Some BullShit
#'
#'
#' @export
topoMap_tf<-function(fn_rstStack=NULL,
                  fn_layerLabel=NULL,
                  fn_label=NULL,
                  fn_prefix='topoMap_',
                  fn_raster=NULL,
                  fn_derivedRaster=NULL,
                  fn_trainingFeatures=NULL,
                  fn_features=NULL,
                  fn_mtry=length(fn_features)/3,
                  fn_ntree=100,
                  fn_trace=10){



  if (is.null(fn_rstStack)) stop(RUNIMC:::mError('no classification specified'))
  if (is.null(fn_layerLabel)) stop(RUNIMC:::mError('no classification layer specified'))
  if (is.null(fn_label)) stop(RUNIMC:::mError('no classification label specified'))
  if (is.null(fn_features)) fn_features<-tf_featureList(fn_trainingFeatures)


  smpCl<-names(fn_rstStack)
  smpRs<-names(fn_raster)
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

      lvls<-raster::levels(fn_rstStack[[smp]][[fn_layerLabel]])[[1]]
      rowLabels<-which(grepl(lbl,lvls$label))
      if (length(rowLabels)==0) {stop(RUNIMC:::mError('unrecognised label'))} else{
        if (length(rowLabels)>1) {message(RUNIMC:::mWarning(paste0(paste0(lvls$label[rowLabels],collapse = ', '), 'will be collapsed to ',lbl)))}
      }
      IDlabels<-lvls$ID[rowLabels]
      labelMask<-lapply(IDlabels,function(x){
        fn_rstStack[[smp]][[fn_layerLabel]]==x
      })
      labelMask<-raster::stack(labelMask)
      labelMask<-raster::calc(labelMask,sum)

      return(labelMask)
    },USE.NAMES = T,simplify = F)
    return(out)
  },USE.NAMES = T,simplify = F)



  # tableOut<-sapply(smpCl,function(smp){
  #
  #
  #   labelOut<-sapply(fn_label,function(lbl){
  #
  #     cat(paste0('Extracting...:::',lbl,'::: for sample ',smp,'\n'))
  #
  #     areaLimits<-fn_area[fn_area[,1]==lbl,]
  #     clmps<-raster::clump(x=classMask[[smp]][[lbl]],directions=fn_clumpDirection,gaps=F)
  #     freqClmps<-raster::freq(clmps)
  #     freqClmps<-freqClmps[freqClmps[,'count']>min(areaLimits[1,2]) & freqClmps[,'count']<max(areaLimits[1,2]),]
  #     DFraster<-raster::as.data.frame(clmps,xy=T)
  #     DFraster<-na.omit(DFraster)
  #     if (fn_maxClumps<nrow(freqClmps)) freqClmps<-freqClmps[sample(1:nrow(freqClmps),fn_maxClumps),]
  #     clmpsMatrix<-pbapply::pblapply(freqClmps[,'value'],function(x){
  #       DFclpm<-DFraster[DFraster$clumps==x,]
  #       xMean<-mean(DFclpm$x)
  #       yMean<-mean(DFclpm$y)
  #       pD<-unname(raster::pointDistance(DFclpm[c('x','y')],c(xMean,yMean),lonlat = F))
  #       cellVector<-raster::cellFromXY(superRaster[[smp]],DFclpm[c('x','y')])
  #       valueMatrix<-raster::extract(x=superRaster[[smp]],y=DFclpm[c('x','y')],cellnumbers=T)
  #       DFclpm<-cbind.data.frame(DFclpm,pD,valueMatrix)
  #     })
  #     clmpsMatrix<-do.call(rbind.data.frame,clmpsMatrix)
  #   },USE.NAMES = T,simplify = F)
  #
  #
  # },USE.NAMES = T,simplify = F)



  # tableOut<-sapply(fn_label,function(lbl){
  #   out<-sapply(smpCl,function(smp){
  #     tableOut[[smp]][[lbl]]
  #   },USE.NAMES = F,simplify = F)
  #   do.call(rbind.data.frame,out)
  # },USE.NAMES = T,simplify = F)



  rf<-sapply(fn_label,function(lbl){
    cat('Random Forest...:::',lbl,':::\n')
    rf<-randomForest::randomForest(x=fn_trainingFeatures$value[,fn_features],
                                   y=fn_trainingFeatures$value[,'DFC'],
                                   mtry=fn_mtry,
                                   ntree=fn_ntree,
                                   do.trace=fn_trace)
    return(rf)
  },USE.NAMES = T,simplify = F)


  classOut<-sapply(smpCl,function(smp){

    labelOut<-sapply(fn_label,function(lbl){


      cat(paste0('predict raster for :::',lbl,'::: in ',smp,'\n'))


      if (is.null(rf[[lbl]])){
        message(RUNIMC:::mWarning('Impossible to produce map for ',lbl,' returning a flat map'))
        outRst<-classMask[[smp]][[lbl]]
        outRst[outRst==0]<-NA
      } else {
        maskedRaster<-raster::mask(x=superRaster[[smp]],
                                   mask = classMask[[smp]][[lbl]],
                                   maskvalue=1,
                                   inverse=T)
        outRst<-raster::predict(maskedRaster,
                                rf[[lbl]],
                                na.rm=T,
                                progress='text')
      }

      names(outRst)<-paste0(fn_prefix,lbl)
      # if (!is.null(fn_filePath)){
      filePath<-raster::filename(fn_rstStack[[smp]][[fn_layerLabel]])
      filePath<-DescTools::SplitPath(filePath)
      fileObjective<-file.path(filePath$drive,filePath$dirname,paste0(fn_prefix,lbl,'.',filePath$extension))
      raster::writeRaster(x = outRst,
                          filename = fileObjective,
                          overwrite=T,
                          format='raster')

      outRst<-raster::raster(fileObjective)
      # }
      return(outRst)
    },USE.NAMES = F,simplify = F)

    names(labelOut)<-unlist(lapply(labelOut,names))

    for (ii in names(labelOut)){
      fn_rstStack[[smp]][[ii]]<-labelOut[[ii]]

    }

    # outStk<-raster::stack(fn_rstStack[[smp]],labelOut)

    # outStk<-RUNIMC:::IMC_stack(x = labelOut,
    #                            uid = fn_rstStack[[smp]]@uid,
    #                            IMC_text_file = fn_rstStack[[smp]]@IMC_text_file,
    #                            study = fn_rstStack[[smp]]@study,
    #                            sample = fn_rstStack[[smp]]@sample,
    #                            replicate = fn_rstStack[[smp]]@replicate,
    #                            ROI = fn_rstStack[[smp]]@ROI,
    #                            bioGroup = fn_rstStack[[smp]]@bioGroup,
    #                            channels = fn_rstStack[[smp]]@channels)

    filePath<-raster::filename(fn_rstStack[[smp]])
    outStk<-IMCstackSave(fn_rstStack[[smp]],filePath)
    return(outStk)
  },USE.NAMES = T,simplify = F)
  return(classOut)
}
