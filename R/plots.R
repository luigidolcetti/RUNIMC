#' @export
ggp.raster<-function(fn_gg = NULL,
                     fn_raster = NULL,
                     fn_colors = NULL,
                     fn_limits = NULL,
                     fn_limitsType = NULL){

  if (is.null(fn_raster)) stop(RUNIMC:::mError("couldn't find a raster to plot"),call. = F)
  if (!is.null(fn_limitsType)) {
    if (!any(fn_limitsType %in% c('fix','quantile','category'))) stop(RUNIMC:::mError("limits type should be: 'fix','quantile' or 'category'"),call. = F)
    if (is.null(fn_limits)) stop(RUNIMC:::mError("specify some limits to use'"),call. = F)
    if (fn_limitsType == 'quantile' & (any(fn_limits<0 | fn_limits>1))) stop(RUNIMC:::mError("for 'quantile' specify limits between 0 and 1"),call. = F)
  } else fn_limitsType  <- 'asIs'
  if (is.null(fn_gg)){

    ggNew<-ggplot2::ggplot()+ggplot2::theme(aspect.ratio = 1,
                                            legend.position = 'none',
                                            panel.background = ggplot2::element_rect(fill = fn_bg),
                                            panel.border = ggplot2::element_blank(),
                                            panel.grid = ggplot2::element_blank()
    )
    # ggNew<-ggNew+ggplot2::coord_equal(ratio = 1)
    # ggNew<-ggplot2::ggplot()+ggplot2::theme(legend.position = 'none')

  } else ggNew<-fn_gg

  DFsubRaster<-raster::as.data.frame(fn_raster,xy=T,long = F)
  lyr<-names(fn_raster)
  colnames(DFsubRaster)<-c('x','y',lyr)
  rasterLevels<-raster::levels(fn_raster)

  if (is.null(rasterLevels)) {
    switch(fn_limitsType,
           fix = {
             scLimits<-fn_limits
           },
           quantile = {
             zeroOff<-DFsubRaster[DFsubRaster[,3]!=0,3]
             scLimits<-stats::quantile(zeroOff,fn_limits)
           },
           category = {
             stop(RUNIMC:::mError("cannot use categorical value with a continuous raster"),call. = F)
           },
           asIs = {
             scLimits<-c(min(DFsubRaster[,3]),max(DFsubRaster[,3]))
           })

    if (is.null(fn_colors)){
      newColors<-c('black','white')
    } else {
      newColors<-fn_colors
    }

    ggNew<-ggNew+ggplot2::geom_raster(data = DFsubRaster,mapping = ggplot2::aes_string(x='x',y='y',fill=lyr),na.rm=T)+
      ggplot2::scale_fill_gradientn(colours = newColors,limits=scLimits,oob=scales::oob_squish)
  } else {

    if (is.null(fn_limits)){
      newLimits<-levels(DFsubRaster[,3])} else {
        if (is.numeric(fn_limits)) {
          newLimits<-rasterLevels[[1]][rasterLevels[,1] %in% fn_limits,2]
        } else {
          newLimits<-fn_limits
        }

      }

    if (is.null(fn_colors)){
      newColors<-RUNIMC:::prettyColors(fn_nSamples = length(levels(DFsubRaster[,3])))
    } else {
      newColors<-fn_colors
    }

    ggNew<-ggNew+ggplot2::geom_raster(data = DFsubRaster,mapping = ggplot2::aes_string(x='x',y='y',fill=lyr),na.rm=T)+
      ggplot2::scale_fill_manual(values = newColors,limits=newLimits)

  }

  return(ggNew)
}


#' @export
ggp.geometry<-function(fn_gg = NULL,
                       fn_geometry = NULL,
                       fn_borderCol = NULL,
                       fn_borderVar = NULL,
                       fn_fillCol = NULL,
                       fn_fillVar = NULL,
                       fn_alpha=1,
                       fn_limits = NULL,
                       fn_bg='white'){

  if (is.null(fn_gg)){

    ggNew<-ggplot2::ggplot()+ggplot2::theme(aspect.ratio = 1,
                                            legend.position = 'none',
                                            panel.background = ggplot2::element_rect(fill = fn_bg),
                                            panel.border = ggplot2::element_blank(),
                                            panel.grid = ggplot2::element_blank()
    )

  } else ggNew<-fn_gg

  if (is.null(fn_geometry)) stop(mError("couldn't find geomety"),call. = F) else {
    colNms<-colnames(fn_geometry)}
  if (!is.null(fn_borderVar)) {
    if (!any(fn_borderVar %in% colNms)) stop(mError("couldn't find border values"))}
  if (!is.null(fn_fillVar)) {
    if (!any(fn_fillVar %in% colNms)) stop(mError("couldn't find fill values"))}

  ggNew<-ggNew+ggnewscale::new_scale(new_aes = c('color','border'))
  ggNew<-ggNew+ggplot2::geom_sf(data=fn_geometry,
                                ggplot2::aes_string(color=fn_borderVar,
                                                    fill=fn_fillVar))

  if (!is.null(fn_limits)){
    ggNew<-ggNew+ggplot2::coord_sf(xlim = fn_limits[1:2],
                                   ylim = fn_limits[3:4],
                                   expand = F,
                                   default = F)
  }

  if (!is.null(fn_fillVar)){
    if (any(class(fn_geometry[[fn_fillVar]]) %in% c('factor','character'))){
      if (is.null(fn_fillCol)) fn_fillCol<-RUNIMC:::prettyColors(fn_nSamples = length(unique(fn_geometry[[fn_fillVar]])))
      ggNew<-ggNew+ggplot2::scale_fill_manual(values = fn_fillCol)}
    if (class(fn_geometry[[fn_fillVar]])=='numeric'){
      if (is.null(fn_fillCol)) fn_fillCol<-c('black','white')
      ggNew<-ggNew+ggplot2::scale_fill_gradientn(colours = fn_fillCol)}
  }




  return(ggNew)
}
