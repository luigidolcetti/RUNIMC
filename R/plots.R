#' Plot IMC data and segmentation
#'
#' [ggp.raster()] can be used to plot a rater image for a single marker.
#'   The image can be overlayed with the product of segmentation using the
#'   function [ggp.geometry()].
#' @param fn_gg ggplot to be overlayed.
#' @param fn_raster rasterLayer.
#' @param fn_colors character vector, colors to be used to construct the gradiant
#'   used for plotting th raster. If NULL, black to white will be used.
#'   Specifying only one color a gradient from black will be constructed.
#' @param fn_limits numeric or character vector, limits of the color gradient,
#'   everything outside will be squished. Permitted values depends on the fn_limitsType
#'   specified.
#' @param fn_limitsType character vector, one among **fix**, **quantile** or
#'   **category**.
#' @param fn_zoom numeric vector, x and y limits of the area to plot.
#' @param fn_title character, title for the plot
#' @param fn_subtitle character vector, a subtitle. A character vector will be
#'   collapsed.
#' @param fn_sep character, character to use for collapse subtitle
#' @param fn_bg character, color to be used in background panel.
#' @param ... not implemented
#' @return ggplot
#' @examples
#' \dontrun{
#' ggp<-ggp.raster(fn_raster = MyStudy$raster[[1]]$X191Ir.DNA1.Ir191Di.,
#'   fn_limits = c(0,0.99),
#'   fn_limitsType = 'quantile',
#'   fn_zoom = c(100,150,100,150),
#'   fn_title = st_uids(MyStudy)[1],
#'   fn_subtitle = c(st_samples(MyStudy)[1],st_rois(MyStudy)[1]))
#'
#' print(ggp)
#' }
#' @export
ggp.raster<-function(fn_raster = NULL,
                     fn_colors = NULL,
                     fn_limits = NULL,
                     fn_limitsType = NULL,
                     fn_zoom=NULL,
                     fn_title = NULL,
                     fn_subtitle = NULL,
                     fn_sep = '/',
                     fn_bg = 'white',
                     ...){

  if (is.null(fn_raster)) stop(RUNIMC:::mError("couldn't find a raster to plot"),call. = F)
  if (!is.null(fn_limitsType)) {
    if (!any(fn_limitsType %in% c('fix','quantile','category'))) stop(RUNIMC:::mError("limits type should be: 'fix','quantile' or 'category'"),call. = F)
    if (is.null(fn_limits)) stop(RUNIMC:::mError("specify some limits to use'"),call. = F)
    if (fn_limitsType == 'quantile' & (any(fn_limits<0 | fn_limits>1))) stop(RUNIMC:::mError("for 'quantile' specify limits between 0 and 1"),call. = F)
  } else fn_limitsType  <- 'asIs'

  ggNew<-ggplot2::ggplot()
  ggNew <- ggNew + ggplot2::theme(legend.title = ggplot2::element_blank(),
                                  legend.position = 'bottom',
                                  panel.background = ggplot2::element_rect(fill = fn_bg),
                                  panel.border = ggplot2::element_blank(),
                                  panel.grid = ggplot2::element_blank())
  if (!is.null(fn_zoom)){
    coordSystem<-ggplot2::coord_fixed(ratio=1,
                                      xlim = c(min(fn_zoom[1:2]),max(fn_zoom[1:2])),
                                      ylim = c(min(fn_zoom[3:4]),max(fn_zoom[3:4])),
                                      expand = F,
                                      clip = 'on')
    #
    coordSystem$default<-T
    ggNew<-ggNew+coordSystem
  }

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
  if (!is.null(fn_title)){
    ggNew<-ggNew+ggplot2::labs(title = paste(fn_title,collapse = fn_sep))
  }
  if (!is.null(fn_subtitle)){
    ggNew<-ggNew+ggplot2::labs(subtitle = paste(fn_subtitle,collapse = fn_sep))
  }

  return(ggNew)
}

#' Plot IMC data and segmentation
#'
#' [ggp.geometry()] can be used to plot the result of segmentation as polygons,
#'   projecting on both border and area any of the continuos of discrete variables
#'   contained in the exprs matrix.
#'   In conjunction with [gg.raster()] overlays polygon outlines over a raster image.
#' @param fn_gg ggplot produced with [gg.raster()] to be overlayed.
#' @param fn_geometry sf dataframe, eg. the table produced after [segment()] and
#' [distillExpression()].
#' @param fn_borderVar character, name of the column to use to project colors
#'   to the border
#' @param fn_borderCol character, colors to use for the border. Can be a single
#'   color, a number of colors equal to the number of categories or a number of
#'   colors to be used to construct a gradient in case of a continuous variable.
#' @param fn_fillVar character, the same as fn_borderVar applied to fill area.
#' @param fn_fillCol character, the same as fn_borderCol applied to fill area.
#' @param fn_zoom numeric vector, x and y limits of the area to plot.
#' @param fn_title character, title for the plot
#' @param fn_subtitle character vector, a subtitle. A character vector will be
#'   collapsed.
#' @param fn_sep character, character to use for collapse subtitle
#' @param fn_bg character, color to be used in background panel.
#' @param ... not implemented
#' @return ggplot
#' @examples
#' \dontrun{
#'
#' sampleToUse <- 1
#' uidTouse<-st_uids(MyStudy)[sampleToUse]
#'
#' ggp<-ggp.raster(fn_raster = MyStudy$raster[[uidTouse]]$X191Ir.DNA1.Ir191Di.,
#'   fn_limits = c(0,0.99),
#'   fn_limitsType = 'quantile',
#'   fn_zoom = c(100,150,100,150),
#'   fn_title = uidTouse,
#'   fn_subtitle = c(st_samples(MyStudy)[sampleToUse],st_rois(MyStudy)[sampleToUse]))
#'
#' ggp<-ggp.geometry(fn_gg = ggp,
#'   fn_geometry = myStudy$currentAnalysis$exprs[
#'     MySudy$currentAnalysis$exprs$uid == uidToUse, ],
#'   fn_borderVar = 'label',
#'   fn_zoom = c(100,150,100,150),
#'   fn_title = st_uids(MyStudy)[1],
#'   fn_subtitle = c(st_samples(MyStudy)[1],st_rois(MyStudy)[1]),
#'   )
#'
#' print(ggp)
#' }
#' @export
ggp.geometry<-function(fn_gg = NULL,
                       fn_geometry = NULL,
                       fn_borderCol = NULL,
                       fn_borderVar = NULL,
                       fn_fillCol = NULL,
                       fn_fillVar = NULL,
                       fn_zoom = NULL,
                       fn_title = NULL,
                       fn_subtitle = NULL,
                       fn_sep = '/',
                       fn_bg='white',
                       ...){

  if (is.null(fn_gg)){

    ggNew<-ggplot2::ggplot()
    ggNew <- ggNew + ggplot2::theme(legend.title = ggplot2::element_blank(),
                                    legend.position = 'bottom',
                                    panel.background = ggplot2::element_rect(fill = fn_bg),
                                    panel.border = ggplot2::element_blank(),
                                    panel.grid = ggplot2::element_blank())


  } else ggNew<-fn_gg

  if (is.null(fn_geometry)) stop(mError("couldn't find geomety"),call. = F) else {
    colNms<-colnames(fn_geometry)}
  if (!is.null(fn_borderVar)) {
    if (!any(fn_borderVar %in% colNms)) stop(RUNIMC:::mError("couldn't find border values"))}
  if (!is.null(fn_fillVar)) {
    if (!any(fn_fillVar %in% colNms)) stop(RUNIMC:::mError("couldn't find fill values"))}


  if (!is.null(fn_gg)){
    ggNew<-ggNew+ggnewscale::new_scale(new_aes = c('color'))
    ggNew<-ggNew+ggplot2::geom_sf(data=fn_geometry,
                                  ggplot2::aes_string(color=fn_borderVar), fill=NA)
  } else {
    ggNew<-ggNew+ggnewscale::new_scale(new_aes = c('color','fill'))
    ggNew<-ggNew+ggplot2::geom_sf(data=fn_geometry,
                                  ggplot2::aes_string(color=fn_borderVar,
                                                      fill=fn_fillVar))
  }

  if (!is.null(fn_zoom)){
    ggNew<-ggNew+ggplot2::coord_sf(xlim = c(min(fn_zoom[1:2]),max(fn_zoom[1:2])),
                                   ylim = c(min(fn_zoom[3:4]),max(fn_zoom[3:4])),
                                   expand = F,
                                   default = F)
  }

  if (!is.null(fn_fillVar)){

    if (class(fn_geometry[[fn_fillVar]]) == 'character'){
      if (is.null(fn_fillCol)) fn_fillCol<-RUNIMC:::prettyColors(fn_nSamples = length(unique(fn_geometry[[fn_fillVar]])))
      ggNew<-ggNew+ggplot2::scale_fill_manual(values = fn_fillCol,drop=F)
    }
    if (class(fn_geometry[[fn_fillVar]]) == 'factor'){
      if (is.null(fn_fillCol)) fn_fillCol<-RUNIMC:::prettyColors(fn_nSamples = length(levels(fn_geometry[[fn_fillVar]])))
      ggNew<-ggNew+ggplot2::scale_fill_manual(values = fn_fillCol,drop=F)
    }
    if (class(fn_geometry[[fn_fillVar]])=='numeric'){
      if (is.null(fn_fillCol)) fn_fillCol<-c('black','white')
      ggNew<-ggNew+ggplot2::scale_fill_gradientn(colours = fn_fillCol)
    }
  } else {
    if (is.null(fn_fillCol) & is.null(fn_gg)){
      fn_fillCol<-'white'
      ggNew<-ggNew+ggplot2::scale_fill_manual(values = fn_fillCol,drop=F)
    }
  }

  if (!is.null(fn_borderVar)){

    if (class(fn_geometry[[fn_borderVar]]) == 'character'){
      if (is.null(fn_borderCol)) fn_borderCol<-RUNIMC:::prettyColors(fn_nSamples = length(unique(fn_geometry[[fn_borderVar]])))
      ggNew<-ggNew+ggplot2::scale_color_manual(values = fn_borderCol,drop=F)
    }
    if (class(fn_geometry[[fn_borderVar]]) == 'factor'){
      if (is.null(fn_borderCol)) fn_borderCol<-RUNIMC:::prettyColors(fn_nSamples = length(levels(fn_geometry[[fn_borderVar]])))
      ggNew<-ggNew+ggplot2::scale_color_manual(values = fn_borderCol,drop=F)
    }
    if (class(fn_geometry[[fn_borderVar]])=='numeric'){
      if (is.null(fn_borderCol)) fn_borderCol<-c('black','white')
      ggNew<-ggNew+ggplot2::scale_color_gradientn(colours = fn_borderCol,drop=F)
    }
  } else {
    if (is.null(fn_borderCol)) fn_borderCol<-'white'
    ggNew<-ggNew+ggplot2::scale_color_manual(values = fn_borderCol,drop=F)
  }



  ggNew <-ggNew + ggplot2::guides(colour = ggplot2::guide_legend(override.aes = list(size=3)))

  if (!is.null(fn_title)){
    ggNew<-ggNew+ggplot2::labs(title = paste(fn_title,collapse = fn_sep))
  }
  if (!is.null(fn_subtitle)){
    ggNew<-ggNew+ggplot2::labs(subtitle = paste(fn_subtitle,collapse = fn_sep))
  }
  return(ggNew)
}

#' Plot IMC data and segmentation
#'
#' Simple scatter or density plot for one, two or three dimensions.
#'
#' @param fn_exprs sf dataframe, eg. the table produced after [segment()] and
#'   [distillExpression()].
#' @param fn_type character, plot type. Can be either scatter or density. For one
#'   dimension, density is the only available.
#' @param fn_x character, column name to plot eleong x axis
#' @param fn_y same as fn_x
#' @param fn_z character, column name to be used as color in a scatter plot.
#' @param fn_xTrsh numeric, draw a vertical line as a threshold (like in flow cytometry).
#' @param fn_yTrsh same as fn_xTrsh
#' @param fn_zLimits numeric vector, boundaries for the dimension represented
#'   as color gradient
#' @param fn_limitisType character, determine how to use fn_zLimits. Can be one
#'   among **fix**, **quantile**.
#' @param fn_xModulus numeric, transformation to apply to x axis, [scales::modulus_trans()].
#' @param fn_yModulus numeric, same as fn_xModulus
#' @param fn_colors character, Can be a single
#'   color, a number of colors equal to the number of categories or a number of
#'   colors to be used to construct a gradient in case of a continuous variable.
#' @param fn_title character, used as title
#' @param fn_subtitle character vector, a subtitle. A character vector will be
#'   collapsed.
#' @param fn_sep character, character to use for collapse subtitle
#' @param fn_bg character, color to be used in background panel.
#' @param fn_fg character, color to be used in accessory graphics on the
#'   foreground.
#' @param ... not implemented
#' @return ggplot
#' @examples
#' \dontrun{
#'
#' }
#'
#' @export
ggp.biplot<-function(fn_exprs = NULL,
                     fn_type = c ('scatter','density'),
                     fn_x = NULL,
                     fn_y = NULL,
                     fn_z = NULL,
                     fn_xTrsh = NULL,
                     fn_yTrsh = NULL,
                     fn_zLimits = NULL,
                     fn_limitsType = NULL,
                     fn_xlimits = NULL,
                     fn_ylimits = NULL,
                     fn_xModulus = 0,
                     fn_yModulus = 0,
                     fn_colors = NULL,
                     fn_title = NULL,
                     fn_subtitle = NULL,
                     fn_sep = '/',
                     fn_bg = 'white',
                     fn_fg = 'black',
                     ...){

  if (is.null(fn_exprs)) stop(RUNIMC:::mError("no data specified"),call. = F)
  fn_type<-match.arg(arg = fn_type,
                     choices = c('scatter','density'))
  if (is.null(fn_x)) stop(RUNIMC:::mError('x must be defined'),call. = F)

  ggNew<-ggplot2::ggplot(data = fn_exprs)
  ggNew <- ggNew + ggplot2::theme(aspect.ratio = 1,
                                  legend.title = ggplot2::element_blank(),
                                  legend.position = 'bottom',
                                  panel.background = ggplot2::element_rect(fill = fn_bg),
                                  # panel.border = ggplot2::element_rect(),
                                  panel.grid = ggplot2::element_line(color = fn_fg,linetype = 3))

  if (!is.null(fn_y)){

    switch(fn_type,
           scatter = {
             ggNew<-ggNew+ggplot2::geom_point(ggplot2::aes_string(x=fn_x,y=fn_y,color=fn_z))
           },
           density = {
             ggNew<-ggNew+ggplot2::geom_density2d(ggplot2::aes_string(x=fn_x,y=fn_y,color=fn_z))
           })

  } else {

    ggNew<-ggNew+ggplot2::geom_density(ggplot2::aes_string(x=fn_x,color=fn_z))

  }


  ggNew<-ggNew + ggplot2::scale_x_continuous(trans = scales::modulus_trans(fn_xModulus),limits = fn_xlimits) +
    ggplot2::scale_y_continuous(trans = scales::modulus_trans(fn_yModulus),limits = fn_ylimits)

  if (!is.null(fn_z)){
    classZ<-class(fn_exprs[[fn_z]])
    nZ<-length(unique(fn_exprs[[fn_z]]))
    if (classZ == 'factor' | classZ == 'character') {
      if (is.null(fn_colors)) {
        newColors<-RUNIMC:::prettyColors(fn_nSamples = nZ)
      } else {
        newColors<-fn_colors
      }
      ggNew<-ggNew+ggplot2::scale_fill_manual(values = newColors,drop=F)
      ggNew<-ggNew+ggplot2::scale_color_manual(values = newColors,drop=F)
    }
    if (classZ == 'numeric') {
      if (is.null(fn_colors)){
        newColors<-c('blue','red')
      }

      if (is.null(fn_limitsType) | is.null(fn_zLimits)) fn_limitsType<-"asIs"
      switch(fn_limitsType,
             fix = {
               scLimits<-fn_zLimits
             },
             quantile = {
               scLimits<-stats::quantile(fn_exprs[[fn_z]],fn_zLimits)
             },
             category = {
               stop(RUNIMC:::mError("cannot set limits on a categorical variable"),call. = F)
             },
             asIs = {
               scLimits<-c(min(fn_exprs[[fn_z]]),max(fn_exprs[[fn_z]]))
             })

      ggNew <- ggNew + ggplot2::scale_color_gradientn(colours = newColors,
                                                      limits=scLimits,
                                                      oob=scales::oob_squish)
    }
  }

  if (!is.null(fn_title)){
    ggNew<-ggNew+ggplot2::labs(title = paste(fn_title,collapse = fn_sep))
  }
  if (!is.null(fn_subtitle)){
    ggNew<-ggNew+ggplot2::labs(subtitle = paste(fn_subtitle,collapse = fn_sep))
  }

  if (!is.null(fn_xTrsh)) ggNew<-ggNew+ggplot2::geom_vline(xintercept = fn_xTrsh,colour=fn_fg)
  if (!is.null(fn_yTrsh)) ggNew<-ggNew+ggplot2::geom_hline(yintercept = fn_yTrsh,colour=fn_fg)

  return(ggNew)
}
