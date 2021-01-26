#' Update study metadata
#'
#' Update metadata in raster collections such as the raw data, additional raster created via filter application and classification rasters.
#'
#' @param x environment, representing a study
#' @param saveToDisk logic, save updated rasterStaks and raster collection
#'
#' @return the method act in place on the correct object within the specified study environment
#' @examples
#' \dontrun{
#' updateMetadata( x = MyStudy, saveToDisk = T)
#' }
#'
#'@export
setGeneric("updateMetadata", function(x,saveToDisk,...)
  standardGeneric("updateMetadata"))


setMethod('updateMetadata',signature = ('environment'),
          function(x,saveToDisk,...){

            if (!is.null(x$raster)){
              x$raster<-.updateMetadata(fn_studyTable = x$studyTable,
                                        fn_rasterCollection = x$raster,
                                        fn_saveToDisk = saveToDisk)

              newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
              attr(x,'mdtnTimeStmp')<-newTimeStmp
              attr(x$raster,'mdtnTimeStmp')<-newTimeStmp
              if (saveToDisk){
                attr(x$raster,'artnTimeStmp')<-newTimeStmp
              }
            }

            if (!is.null(x$currentAnalysis$derivedRaster)){
              x$currentAnalysis$derivedRasters<-.updateMetadata(fn_studyTable = x$studyTable,
                                                                fn_rasterCollection = x$currentAnalysis$derivedRasters,
                                                                fn_saveToDisk = saveToDisk)

              newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
              attr(x,'mdtnTimeStmp')<-newTimeStmp
              attr(x$currentAnalysis$derivedRasters,'mdtnTimeStmp')<-newTimeStmp
              if (saveToDisk){
                attr(x$currentAnalysis$derivedRasters,'artnTimeStmp')<-newTimeStmp
              }
            }

            if (!is.null(x$currentAnalysis$classification)){
              x$currentAnalysis$classification<-.updateMetadata(fn_studyTable = x$studyTable,
                                                                fn_rasterCollection = x$currentAnalysis$classification,
                                                                fn_saveToDisk = saveToDisk)

              newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
              attr(x,'mdtnTimeStmp')<-newTimeStmp
              attr(x$currentAnalysis$classification,'mdtnTimeStmp')<-newTimeStmp
              if (saveToDisk){
                attr(x$currentAnalysis$classification,'artnTimeStmp')<-newTimeStmp
              }
            }
          })




.updateMetadata<-function(fn_studyTable=NULL,
                          fn_rasterCollection=NULL,
                          fn_saveToDisk=T){

  if (is.null(fn_studyTable)) stop(RUNIMC:::mError('need study table'))
  if (is.null(fn_rasterCollection)) stop(RUNIMC:::mError('need raster collection'))

  uids<-names(fn_rasterCollection)

  for (uid in uids){
    fn_rasterCollection[[uid]]@sample<-fn_studyTable$sample[fn_studyTable$uid==uid]
    fn_rasterCollection[[uid]]@replicate<-fn_studyTable$replicate[fn_studyTable$uid==uid]
    fn_rasterCollection[[uid]]@ROI<-fn_studyTable$ROI[fn_studyTable$uid==uid]
    fn_rasterCollection[[uid]]@bioGroup<-fn_studyTable$bioGroup[fn_studyTable$uid==uid]
    if (fn_saveToDisk){
      fileTarget<-fn_rasterCollection[[uid]]@filename
      if (!is.null(fileTarget)){
        fn_rasterCollection[[uid]]<-IMCstackSave(fn_rasterCollection[[uid]],fileTarget)
        newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
        attr(fn_rasterCollection[[uid]],'mdtnTimeStmp')<-newTimeStmp
        attr(fn_rasterCollection[[uid]],'artnTimeStmp')<-newTimeStmp
        attr(fn_rasterCollection[[uid]],'fileArchive')<-fileTarget
      } else {meassage(RUNIMC:::mWarning('cannot save, object needs to be properly archived first'))}
    }
  }
  return(fn_rasterCollection)
}


