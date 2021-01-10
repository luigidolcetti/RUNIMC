#'Internal function to convert a txt file to a RasterStack usually executed during study initialisation.
#'
#' @param fn_path Root path for a bunch of .txt image files
#' @export
loadTxtToStack<-function(fn_path,
                         fn_file,
                         fn_rasterDBPath=NULL,
                         fn_rasterStackPath=NULL,
                         fn_cols=NULL,
                         fn_newNames=NULL,
                         fn_norm=F,
                         fn_details=list(study='NO_study',sample='NO_sample',ROI='NO_ROI', replicate='NO_replicate', bioGroup='No_bioGroup'),
                         fn_channel=NULL,
                         fn_trsh=0.965,
                         fn_zeroOff=T){

  if (!all(names(fn_details) %in% c('study','sample','ROI','replicate','bioGroup'))) {stop("Please, details accepts only 'study', 'sample', 'replicate', 'ROI', 'bioGroup")}
  if (!dir.exists(fn_path)) {stop("could not find path")}
  if (!file.exists(paste(fn_path,fn_file,sep='/'))) {stop("could not find file")}
  if (is.null(fn_rasterDBPath)) {stop("Please, specifify a path for storing raster files")}

  rawMatrix<-utils::read.table(paste(fn_path,fn_file,sep='/'),sep='\t',header = T,check.names = F)


  xCoords<-list(MIN=min(rawMatrix$X),MAX=max(rawMatrix$X))
  yCoords<-list(MIN=min(rawMatrix$Y),MAX=max(rawMatrix$Y))
  zCoords<-list(MIN=min(rawMatrix$Z),MAX=max(rawMatrix$Z))

  rasterMatrix<-lapply(fn_cols,
                       function(cls){

                         singleMatrix<-array(data=as.matrix(rawMatrix[,cls]),dim = c(xCoords$MAX+1,yCoords$MAX+1))
                         if (fn_norm){
                           singleMatrix<-quantNorm(singleMatrix,fn_trsh,fn_zeroOff)}
                         rst<-raster::raster(singleMatrix,
                                             xmn=0,
                                             xmx=ncol(singleMatrix),
                                             ymn=0,
                                             ymx=nrow(singleMatrix),
                                             crs =  sp::CRS(as.character(NA)))
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
    cat(paste0(fn_file,'.stk',' saved in ',fn_rasterStackPath))
  }
  return(rstrStk)
}
