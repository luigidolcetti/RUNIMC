#' From txt to rasterStack
#'
#' [loadTxtToStack()] is an internal function, used by [initStudy()] to convert a txt file to a RasterStack.
#'
#' @param fn_path Root path for a bunch of .txt image files.
#' @param fn_file .txt file name containing raw data.
#' @param fn_rasterDBPath target folder where to store raster object.
#' @param fn_rasterStackPath target folder where to store rasterStack files.
#' @param fn_cols vector of column names to load.
#' @param fn_newNames vector of nemes to use as layer names.
#' @param fn_norm normalise raw data using quantile after stripping all 0-values.
#' @param fn_details list of experimental details, possible names are study = study name, sample, ROI, replicate, bioGroup.
#'   These information can be updated via [updateMetadata()].
#' @param fn_channel IMC_ChannelTable object.
#' @param fn_trsh quantile as the high treshold used for normalisation.
#' @param fn_zeroOff logical whether or not to strip 0-value in the process of normalisation.
#'
#' @return an IMC_RasterStack object
#'
#' @export
loadTxtToStack<-function(fn_path=NULL,
                         fn_file=NULL,
                         fn_rasterDBPath=NULL,
                         fn_rasterStackPath=NULL,
                         fn_cols=NULL,
                         fn_newNames=NULL,
                         fn_transpose=F,
                         fn_norm=F,
                         fn_details=list(study='NO_study',sample='NO_sample',ROI='NO_ROI', replicate='NO_replicate', bioGroup='No_bioGroup'),
                         fn_channel=NULL,
                         fn_trsh=0.965,
                         fn_zeroOff=T,
                         fn_verbose=T){

  if (!all(names(fn_details) %in% c('study','sample','ROI','replicate','bioGroup'))) {stop("Please, details accepts only 'study', 'sample', 'replicate', 'ROI', 'bioGroup")}
  if (!dir.exists(fn_path)) {stop("could not find path")}
  if (!file.exists(paste(fn_path,fn_file,sep='/'))) {stop("could not find file")}
  if (is.null(fn_rasterDBPath)) {stop("Please, specifify a path for storing raster files")}

  rawMatrix<-utils::read.table(paste(fn_path,fn_file,sep='/'),sep='\t',header = T,check.names = F)


  rasterMatrix<-lapply(fn_cols,
                       function(cls){

                         singleMatrix<-array(rawMatrix[,c('X','Y',cls)])

                         if (fn_norm){
                           singleMatrix[,cls]<-quantNorm(singleMatrix[,cls],fn_trsh,fn_zeroOff)
                           }

                         rst<-raster::rasterFromXYZ(singleMatrix,
                                                    res = 1,
                                                    crs = sp::CRS(as.character(NA)))

                         if (fn_transpose) rst<-raster::t(rst)

                         names(rst)<-fn_newNames[fn_cols==cls]
                         fileObjective<-paste0(fn_rasterDBPath,'/',names(rst),'.nc')
                         raster::writeRaster(x = rst,
                                             filename = fileObjective,
                                             overwrite=T,
                                             format='CDF')
                         return(fileObjective)})

  rstrStk<-IMC_stack(x = rasterMatrix,
                     uid = digest::digest(rawMatrix,seed=123),
                     IMC_text_file = fn_file,
                     study = fn_details$study,
                     sample = fn_details$sample,
                     replicate = fn_details$replicate,
                     ROI = fn_details$ROI,
                     bioGroup = fn_details$bioGroup,
                     channels = fn_channel)


  if (!is.null(fn_rasterStackPath)) {
    rstrStk<-IMCstackSave(rstrStk,paste0(fn_rasterStackPath,'/',fn_file,'.stk'))
    if (fn_verbose) message(RUNIMC:::mMessage(paste0(fn_file,'.stk',' saved in ',fn_rasterStackPath)))
  }

  return(rstrStk)
}
