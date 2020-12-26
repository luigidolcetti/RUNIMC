ggRaster<-function(fn_raster=NULL,
                   fn_colors=NULL,
                   fn_labels=NULL,
                   fn_treshMod='quantile',
                   fn_tresh=0.996,
                   fn_studyTable=NULL,
                   fn_ID=NULL,
                   fn_sample=NULL,
                   fn_ROI=NULL,
                   fn_marker=NULL,
                   fn_lable=NULL,
                   fn_scale=T,
                   fn_save=F){
  
  browser()
  if (is.null(fn_raster)) stop(RUNIMC:::mError("Specify a raster"))
  if (class(fn_raster)=='RasterLayer'){
    DFraster<-raster::rasterToPoints(fn_raster)
    colnames(DFraster)<-c('x','y','z')
    DFraster<-as.data.frame(DFraster)
    lvls<-raster::levels(fn_raster)[[1]]
    if (!is.null(lvls)) {
      plotBrakes<-lvls[,1]
      plotLabels<-as.character(lvls[,2])
      scrmbLabels<-strsplit(plotLabels,"_")
      scrmbLabels<-unlist(lapply(scrmbLabels,function(x){
        paste0(x[2],' (',x[1],')')
      }))
      DFraster$z<-factor(x = DFraster$z,
                         levels =plotBrakes,
                         labels = scrmbLabels)
      if (!is.null(fn_labels)){
        if (!all(fn_labels %in% plotLabels)) {stop(RUNIMC:::mError("Lables don't match"))} else {
          fn_labels<-lvls[lvls[,2] %in% fn_labels,1]
          DFraster<-DFraster[as.numeric(DFraster$z) %in% fn_labels,]
        }
        
      }
      
      if (is.null(fn_colors)) fn_colors<-RUNIMC:::prettyColors(length(plotBrakes))
      ggp<-ggplot2::ggplot(as.data.frame(DFraster),ggplot2::aes(x=x,y=y,fill=z))+ggplot2::geom_raster()
      
      if (!is.null(fn_sample) & !is.null(fn_ROI)){
        ggp<-ggp+ggplot2::ggtitle(label = fn_sample,subtitle = fn_ROI)
      }
      if (is.null(fn_sample) & !is.null(fn_ROI)){
        ggp<-ggp+ggplot2::ggtitle(label = fn_ROI)
      }
      if (!is.null(fn_sample) & is.null(fn_ROI)){
        ggp<-ggp+ggplot2::ggtitle(label = fn_sample)
      }
      ggp<-ggp+ggplot2::labs(fill="label")
      ggp<-ggp+ggplot2::scale_fill_manual(values = fn_colors)
      ggp<-ggp+ggplot2::theme_minimal(base_size = 8)
      return(ggp)
    } else {
      if (is.null(fn_tresh)){
        trsh<-max(DFraster$z)
      } else {
        if (fn_treshMod=="quantile"){
          trsh<-quantile(DFraster$z[DFraster$z!=0],fn_tresh)}
        if (fn_treshMod=="fix"){
          trsh<-fn_tresh
        }  
      }
      ggp<-ggplot2::ggplot(as.data.frame(DFraster),ggplot2::aes(x=x,y=y,fill=z))+
        ggplot2::geom_raster()
      if (!is.null(fn_marker)) {
        ggp<-ggp+ggplot2::labs(fill=fn_marker)
      } else {
        ggp<-ggp+ggplot2::labs(fill="Intensity")}
      if (!is.null(fn_sample) & !is.null(fn_ROI)){
        ggp<-ggp+ggplot2::ggtitle(label = tit,subtitle = ROI)
      }
      if (is.null(fn_sample) & !is.null(fn_ROI)){
        ggp<-ggp+ggplot2::ggtitle(label = ROI)
      }
      if (!is.null(fn_sample) & is.null(fn_ROI)){
        ggp<-ggp+ggplot2::ggtitle(label = fn_sample)
      }
      if (is.null(fn_colors)) fn_colors<-c('black','white')
      
      ggp<-ggp+ggplot2::scale_fill_gradientn(colours = fn_colors,
                                             guide = 'colorbar',
                                             aesthetics = 'fill',
                                             limits=c(0,trsh),
                                             oob=scales::squish)
      ggp<-ggp+ggplot2::theme_minimal(base_size = 8)
    } 
  }
  
  
}


mixMod<-function(fn_raster){
  # browser()
  if (is.null(fn_raster)) stop(RUNIMC:::mError("Specify a raster"))
  if (class(fn_raster)=='RasterLayer'){
    DFraster<-raster::rasterToPoints(fn_raster)
    colnames(DFraster)<-c('x','y','z')
    DFraster<-as.data.frame(DFraster)
    smp<-DFraster$z[sample(1:nrow(DFraster),100)]
    # shp<-shapiro.test(smp)
    # egm<-EnvStats::egamma(smp)
    # gmdist<-rgamma(1000,shape = egm$parameters[1],scale = egm$parameters[2])
    # ks.test(smp,gmdist)
    # if (shp$p.value<0.001){
    # mt<-mixtools::normalmixEM(smp)
    browser()
    mt<-mclust::Mclust(smp,G=2,modelNames = 'E')
    prmt<-summary(mt,parameters = T)
    whichMu<-which(prmt$mean==max(prmt$mean))
    treshold<-qnorm(c(0.005,0.995),mean = prmt$mean[whichMu],sd=prmt$variance[whichMu]) # } else {
    # treshold<-(quantile(smp,0.99))^2
    # }
    return(treshold)
  }
  
}

mixMod_new<-function(fn_raster){
  browser()
  if (is.null(fn_raster)) stop(RUNIMC:::mError("Specify a raster"))
  if (class(fn_raster)=='RasterLayer'){
    DFraster<-raster::rasterToPoints(fn_raster)
    colnames(DFraster)<-c('x','y','z')
    DFraster<-as.data.frame(DFraster)
    DFz<-DFraster$z[DFraster$z!=0]
    treshold<-quantile(DFz,0.995)
    return(treshold)
  }
  
}


mixMod_nn<-function(fn_raster){
  browser()
  if (is.null(fn_raster)) stop(RUNIMC:::mError("Specify a raster"))
  if (class(fn_raster)=='RasterLayer'){
    DFraster<-raster::rasterToPoints(fn_raster)
    colnames(DFraster)<-c('x','y','z')
    DFraster<-as.data.frame(DFraster)
    DFraster<-DFraster[DFraster$z!=0,]
    DFraster<-as.data.frame(DFraster[sample(1:nrow(DFraster),1000),])
    fitdistrplus::descdist(DFraster$z,discrete = F)
    ft<-fitdistrplus::fitdist(DFraster$z,'norm')
    return(treshold)
  }
  
}
