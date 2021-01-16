#'
#'
#'@export
.importTiffMask<-function(fn_fileList = NULL,
                         fn_transpose = T,
                         fn_layerName = 'Tiff_imported'){

  lrst <- lapply(fn_fileList, function(dataFile){
    rst<-raster::raster(dataFile)
    if (fn_transpose) rst<-raster::t(rst)
    names(rst)<-fn_layerName
    return(rst)
  })

  return(lrst)
}



if (!isGeneric("importTiffMask")) {
  setGeneric("importTiffMask", function(x,fileFolder=NULL,orderAgain=NULL,transposeImage=T,layerName='Tiff_imported',saveToDisk=T, ...)
    standardGeneric("importTiffMask"))
}


#'
#'
#' @export
setMethod('importTiffMask',signature = ('environment'),
          function(x,fileFolder=NULL,orderAgain=NULL,transposeImage=T,layerName='Tiff_imported',saveToDisk=T, ...){


            if (is.null(fileFolder)) {stop(RUNIMC:::mError('specify a folder where to look for .tiff'))}
            if (!dir.exists(fileFolder)) {stop(RUNIMC:::mError('cannot find specified folder'))}
            if (is.null(orderAgain)) {message(RUNIMC:::mWarning('an order has not been specified so files are supposed to be in the correct order'))}

            uids<-x$studyTable$uid
            filesToImport<-list.files(fileFolder,full.names = T )

            if (length(uids)!=length(filesToImport)) {stop(RUNIMC:::mError('the number of files in this folder does not match the number of samples for this study'))}
            filesToImport<-filesToImport[orderAgain]

            newClassification<-.importTiffMask(fn_fileList = filesToImport)

            for (i in 1:length(newClassification)){
              comparisonRst<-raster::compareRaster(newClassification[[i]],
                                                   x$raster[[i]],
                                                   extent = T,
                                                   rowcol = T,
                                                   crs = T,
                                                   stopiffalse = F,
                                                   res = T)
              if (!comparisonRst){stop(RUNIMC:::mError('raster and masks do not match'))}
            }

            names(newClassification)<-uids

            if (saveToDisk) {
              filePath<-file.path(x$currentAnalysis$folder,
                                     'test/classification')
              rasterFilePath<-checkDir(filePath,'rasters')
              stackFilePath<-checkDir(filePath,'rasterStacks')
            }

            newClassification<-sapply(uids,function(uid){
              if (saveToDisk) {

                TXT_file<-x$studyTable$IMC_text_file[x$studyTable$uid==uid]
                filePath<-checkDir(rasterFilePath,TXT_file)
                filePath<-file.path(filePath,paste0(layerName,'.grd'))
                rst<-raster::writeRaster(x=newClassification[[uid]],
                                         filename = filePath,
                                         overwrite=T,
                                         format='raster')

                rstrStk<-IMC_stack(x = list(rst),
                                   uid = uid,
                                   IMC_text_file = TXT_file,
                                   study = x$studyTable$study[x$studyTable$uid==uid],
                                   sample = x$studyTable$sample[x$studyTable$uid==uid],
                                   replicate = x$studyTable$replicate[x$studyTable$uid==uid],
                                   ROI = x$studyTable$ROI[x$studyTable$uid==uid],
                                   bioGroup = x$studyTable$bioGroup[x$studyTable$uid==uid],
                                   channels = data.frame())

                names(rstrStk)<-layerName

                rstrStk<-IMCstackSave(rstrStk,file.path(stackFilePath,paste0(TXT_file,'.stk')))
              } else {
                rstrStk<-IMC_stack(x = newClassification[[x]],
                                   uid = uid,
                                   IMC_text_file = TXT_file,
                                   study = x$studyTable$study[x$studyTable$uid==uid],
                                   sample = x$studyTable$sample[x$studyTable$uid==uid],
                                   replicate = x$studyTable$replicate[x$studyTable$uid==uid],
                                   ROI = x$studyTable$ROI[x$studyTable$uid==uid],
                                   bioGroup = x$studyTable$bioGroup[x$studyTable$uid==uid],
                                   channels = data.frame())
              }
              return(rstrStk)
              },USE.NAMES = T,simplify = F)



            newClassification<-new('IMC_Classification',newClassification)
            x$currentAnalysis$classification<-newClassification

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
            if (saveToDisk) {
              attr(x$currentAnalysis$classification,'artnTimeStmp')<-newTimeStmp
              attr(x$currentAnalysis$classification,'fileArchive')<-file.path(x$currentAnalysis$folder,'test/classification/rasterStacks')
            }








          })

