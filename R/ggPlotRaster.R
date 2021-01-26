#' @export
plot.raster<-function(fn_raster=NULL,
                      fn_layer=NULL,
                      fn_colorsContinuous=NULL,
                      fn_scaleColourLimits=NULL,
                      fn_limitsType=NULL,
                      fn_colorsDiscrete=NULL,
                      fn_labels=NULL,
                      fn_segmentation=NULL,
                      fn_segmentLabel=NULL,
                      fn_segmentColor=NULL,
                      fn_zoom=NULL){



  if (class(fn_raster)!='IMC_RasterStack') stop(RUNIMC:::mError('object is not an IMC_RasterStack'))

  if (is.null(fn_layer)) fn_layer<-names(fn_raster)

  if (!is.null(fn_limitsType)) {
    if (!any(fn_limitsType %in% c('quantile','fix'))) stop(RUNIMC:::mError('limits type must be quantile or fix'))
  }
  if (is.null(fn_zoom)) coordLim<-raster::extent(fn_raster) else {
    coordLim<-c(fn_zoom[1]-fn_zoom[2]/2,
                fn_zoom[1]+fn_zoom[2]/2,
                fn_zoom[3]-fn_zoom[4]/2,
                fn_zoom[3]+fn_zoom[4]/2)}

  labelExt<-paste0(fn_raster@uid,'\n',
                   fn_raster@sample,'\n',
                   fn_raster@replicate,'\n',
                   fn_raster@ROI,'\n')


  subRaster<-fn_raster[[fn_layer]]
  if (is.null(fn_limitsType) & !is.null(fn_scaleColourLimits)) stop(RUNIMC:::mError('specify limits type'))
  if (is.null(fn_limitsType) & is.null(fn_scaleColourLimits)) {
    fn_limitsType<-'fix'
    fn_scaleColourLimits<-rep(c(NA,NA),raster::nlayers(subRaster))
  }
  if (!is.null(fn_scaleColourLimits)){
    if (length(fn_scaleColourLimits)==2){
      fn_scaleColourLimits<-rep(fn_scaleColourLimits,raster::nlayers(subRaster))
    }
    if (length(fn_scaleColourLimits)<2*length(fn_raster)){
      fn_scaleColourLimits<-c(fn_scaleColourLimits,rep(c(NA,NA),(raster::nlayers(subRaster))-(length(fn_scaleColourLimits)/2)))
    }
  }

  DFsubRaster<-raster::as.data.frame(subRaster,xy=T,long = F)
  colnames(DFsubRaster)<-c('x','y',fn_layer)
  # PRB: layer to plot should be numeric or factor, if character labels then reformat as factor locally
  if(class(DFsubRaster[,fn_layer])=='character'){
    DFsubRaster[,fn_layer] <- as.factor(DFsubRaster[,fn_layer])
  }
  # if integer, cast as numeric
  if(class(DFsubRaster[,fn_layer])=='integer'){
    DFsubRaster[,fn_layer] <- as.numeric(DFsubRaster[,fn_layer])
  }

  ggList<-sapply(fn_layer,function(lyr){

    lyrIndex<-which(fn_layer==lyr)*2
    labelLyr<-paste0(labelExt,lyr)

    if (class(DFsubRaster[,lyr])=='numeric'){
      if (is.null(fn_colorsContinuous)) colorsContinuous<-c('black','white') else colorsContinuous<-fn_colorsContinuous
      ggp<-ggplot2::ggplot()
      ggp<-ggp+ggplot2::geom_raster(data = DFsubRaster,mapping = ggplot2::aes_string(x='x',y='y',fill=lyr))
      # ggp<-ggp+ggplot2::coord_fixed(ratio = 1,xlim = coordLim[1:2],ylim = coordLim[3:4])
      if (fn_limitsType=='fix'){
        if (is.na(fn_scaleColourLimits[lyrIndex])) fn_scaleColourLimits[lyrIndex]<-max(DFsubRaster[,lyr])
        if (is.na(fn_scaleColourLimits[lyrIndex-1])) fn_scaleColourLimits[lyrIndex-1]<-min(DFsubRaster[,lyr])
        scLimits<- fn_scaleColourLimits[c(lyrIndex-1,lyrIndex)]
      }
      if (fn_limitsType=='quantile'){
        if (is.na(fn_scaleColourLimits[lyrIndex])) fn_scaleColourLimits[lyrIndex]<-100
        if (is.na(fn_scaleColourLimits[lyrIndex-1])) fn_scaleColourLimits[lyrIndex-1]<-0
        scLimits<-quantile(DFsubRaster[,lyr][DFsubRaster[,lyr]!=0],fn_scaleColourLimits[c(lyrIndex-1,lyrIndex)])
      }
      if (is.null(fn_limitsType)){
        scLimits<-c(min(DFsubRaster[,lyr]),max(DFsubRaster[,lyr]))
      }
      ggp<-ggp+ggplot2::scale_fill_gradientn(colours = colorsContinuous,limits=scLimits,oob=scales::oob_squish)
      ggp<-ggp+ggplot2::labs(fill=labelLyr)
    }
    if (class(DFsubRaster[,lyr])=='factor'){

      lvls<-(levels(DFsubRaster[,lyr]))
      if (is.null(fn_colorsDiscrete)) colorsDiscrete<-RUNIMC:::prettyColors(length(levels(DFsubRaster[,lyr]))) else{
        if (length(lvls)>length(fn_colorsDiscrete)) stop(RUNIMC:::mError(paste0(length(fn_colorsDiscrete),' colors specified but need ',length(lvls))))
        colorsDiscrete<-fn_colorsDiscrete}

      if (is.null(fn_labels)) fn_labels<-lvls
      if (!is.null(fn_labels)){
        if (!all(fn_labels %in% lvls)){
          message(RUNIMC:::mWarning("some labels don't match and will be ignored"))
        fn_labels<-lvls}
      }
      ggp<-ggplot2::ggplot()
      ggp<-ggp+ggplot2::geom_raster(data = DFsubRaster,mapping = ggplot2::aes_string(x='x',y='y',fill=lyr))
      # ggp<-ggp+ggplot2::coord_fixed(ratio = 1,xlim = coordLim[1:2],ylim = coordLim[3:4])

      ggp<-ggp+ggplot2::scale_fill_manual(values = colorsDiscrete,limits=fn_labels)
      ggp<-ggp+ggplot2::labs(labelLyr)
    }

    ggp<-ggp+ggplot2::theme_minimal()

    if (!is.null(fn_segmentation)){
      allLabels<-unique(fn_segmentation$label)
      if (is.null(fn_segmentLabel)) fn_segmentLabel<-allLabels
      subSegmentation<-fn_segmentation[fn_segmentation$uid==fn_raster@uid,]
      subSegmentation<-subSegmentation[subSegmentation$label %in% fn_segmentLabel,]
      if (is.null(fn_segmentColor)) {
        fn_segmentColor<-RUNIMC:::prettyColors(length(allLabels))
      names(fn_segmentColor)<-fn_segmentLabel
      } else {
        if(length(fn_segmentColor)<length(fn_segmentLabel)) stop(RUNIMC:::mError('fewer colors than segmentation labels'))
        if(length(fn_segmentColor)>length(fn_segmentLabel)) fn_segmentColor<-fn_segmentColor[1:length(fn_segmentLabel)]
        if(is.null(names(fn_segmentColor))) names(fn_segmentColor)<-fn_segmentLabel
      }

      ggp<-ggp+ggnewscale::new_scale(new_aes = 'color')
      ggp<-ggp+ggplot2::geom_sf(data=subSegmentation,ggplot2::aes(color=label),fill=NA)
      ggp<-ggp+ggplot2::coord_sf(xlim = coordLim[1:2],ylim = coordLim[3:4])
      ggp<-ggp+ggplot2::scale_color_manual(values=fn_segmentColor,breaks =fn_segmentLabel)
    } else {
      ggp<-ggp+ggplot2::coord_fixed(ratio = 1,xlim = coordLim[1:2],ylim = coordLim[3:4])

    }
    attr(ggp,'uid') <- fn_raster@uid
    attr(ggp,'sample') <- fn_raster@sample
    attr(ggp,'replicate')<-fn_raster@replicate
    attr(ggp,'ROI')<-fn_raster@ROI

    return(ggp)
  },USE.NAMES = T,simplify = F)

  return(ggList)
}
