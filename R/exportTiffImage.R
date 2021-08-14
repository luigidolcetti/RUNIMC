#' Export Tiff image
#'
#'
#' @param fn_Raster IMC_RasterStack
#' @return
#' @examples
#'
#' \dontrun{
#' }
#'
#' @export
.exportTiffImage<-function(fn_Raster = NULL,
                           fn_Layers = NULL,
                           fn_NAVal = 0,
                           fn_transpose = F,
                           fn_flip = c('NA','x','y','xy'),
                           fn_scale = F,
                           fn_path = NULL,
                           fn_fileName = NULL,
                           fn_bps = c('32','16','8'),
                           fn_verbose = T){


  lyr<-raster::labels(fn_Raster)

  if (is.null(fn_Layers)) fn_Layers<-lyr
  if (!all(fn_Layers %in% lyr)) stop(RUNIMC:::mError("not all specified layers belong to this raster"),call. = F)

  fn_flip<-match.arg(fn_flip)
  fn_bps<-match.arg(fn_bps)

  if (is.null(fn_path)) fn_path<-tempdir()
  if (is.null(fn_fileName)) {
    newPath<-tempfile(tmpdir = fn_path)} else {
      newPath<-file.path(fn_path,fn_fileName)
    }

  dir.create(newPath)

  # outRst<-fn_Raster[[fn_Layers]]

  for (lyr in fn_Layers){
    outRst<-raster::readAll(fn_Raster[[lyr]])
    if (fn_transpose) outRst<-raster::t(outRst)
    switch (fn_flip,
            x = {outRst<-raster::flip(outRst,fn_flip)},
            y = {outRst<-raster::flip(outRst,fn_flip)},
            xy = {
              outRst<-raster::flip(outRst,direction = 'x')
              outRst<-raster::flip(outRst,direction = 'y')
            })
    if (fn_scale) {
          minx<-raster::minValue(outRst)
          maxx<-raster::maxValue(outRst)
          outRst<-(outRst-minx)/(maxx-minx)
        }

    val<-raster::values(outRst)
    val[is.na(val)]<-fn_NAVal
    raster::values(outRst)<-val

    raster::writeRaster(outRst,
                        filename = file.path(newPath,paste0('Export_',lyr)),
                        format = 'GTiff',
                        bylayer = T,
                        overwrite = T)
  }
  if (fn_verbose) print(paste0('Tiff file saved in ',newPath))

  return(newPath)
}


#' Export Tiff image
#'
#'
#' @param x environment, a study
#' @param fileFolder root folder containing at least a folder (which name is gone to be used as layer name), containing one tiff mask for each sample
#' @param ... not implemented
#'
#' @return none
#'
#' @examples
#'
#' \dontrun{
#' }
#'
#' @export
setGeneric("exportTiffImage", function(x,
                                       what = c('raw','derived','classification'),
                                       layerNames=NULL,
                                       NAVal = 0,
                                       uids = NULL,
                                       fileFolder=NULL,
                                       transposeImage = F,
                                       flipImage = c('NA','x','y','xy'),
                                       scaleImage = F,
                                       bitsImage = c('32','16','8'),
                                       verbose=T,
                                       ...)

  standardGeneric("exportTiffImage"))

setMethod('exportTiffImage',signature = ('environment'),
          function(x,
                   what = c('raw','derived','classification'),
                   layerNames=NULL,
                   uids = NULL,
                   fileFolder=NULL,
                   transposeImage = F,
                   flipImage = c('NA','x','y','xy'),
                   scaleImage = F,
                   bitsImage = c('32','16','8'),
                   verbose=T,
                   ...){


            if (is.null(fileFolder)) {stop(RUNIMC:::mError('specify a folder where to look for .tiff'))}
            if (!dir.exists(fileFolder)) {stop(RUNIMC:::mError('cannot find specified folder'))}

            if (is.null(uids)) uids<-x$studyTable$uid

            what<-match.arg(what)
            flipImage<-match.arg(flipImage)
            bitsImage<-match.arg(bitsImage)

            switch(what,

                   raw = out<-x$raster[uids],
                   derived = out<-x$currentAnalysis$derivedRasters[uids],
                   classification = out<-x$currentAnalysis$classification[uids])

            lyrNms<-raster::labels(out)

            if (!is.null(layerNames)){
              if (!all(layerNames %in% lyrNms)) {
                stop(RUNIMC:::mError("some of the specified layers do not match the current layers"),call. = F)} else {
                }
              out<-lapply(uids,function(u){
                out[[u]][layerNames]
              })
            }

            for (u in uids){
              RUNIMC:::.exportTiffImage(fn_Raster = out[[u]],
                                        fn_Layers = layerNames,
                                        fn_NAVal = NAVal,
                                        fn_transpose = transposeImage,
                                        fn_flip = flipImage,
                                        fn_scale = scaleImage,
                                        fn_path = fileFolder,
                                        fn_fileName = out[[u]]@IMC_text_file,
                                        fn_bps = bitsImage,
                                        fn_verbose = verbose)
            }
          })

