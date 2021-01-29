#' @export
randomOnions<-function(fn_rstStack=NULL,
                       fn_layerLabel=NULL,
                       fn_raster = NULL,
                       fn_derivedRaster = NULL,
                       fn_label=NULL,
                       fn_classifiers=NULL,
                       fn_prefix='topoMap_'){

    if (is.null(fn_layerLabel)) stop(RUNIMC:::mError('no classification layer specified'))
    if (is.null(fn_label)) stop(RUNIMC:::mError('no classification label specified'))


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


    ##############################################
    classOut<-sapply(smpCl,function(smp){

      labelOut<-sapply(fn_label,function(lbl){


        cat(paste0('predict raster for :::',lbl,'::: in ',smp,'\n'))


        if (is.null(fn_classifiers[[lbl]])){
          message(RUNIMC:::mWarning('Impossible to produce map for ',lbl,' returning a flat map'))
          outRst<-classMask[[smp]][[lbl]]
          outRst[outRst==0]<-NA
        } else {
          maskedRaster<-raster::mask(x=superRaster[[smp]],
                                     mask = classMask[[smp]][[lbl]],
                                     maskvalue=1,
                                     inverse=T)
          outRst<-raster::predict(maskedRaster,
                                  fn_classifiers[[lbl]],
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

      filePath<-raster::filename(fn_rstStack[[smp]])
      outStk<-IMCstackSave(fn_rstStack[[smp]],filePath)
      return(outStk)
    },USE.NAMES = T,simplify = F)
    return(classOut)
  }
