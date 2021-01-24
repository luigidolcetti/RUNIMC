#' Import tiff Mask
#'
#' Import a classification map from a different source
#'
#' @param fn_fileList character vector, a list of tiff files to be imported
#' @param fn_transpose logical, transpose matrix? It should be if the raw data has been transposed as well
#' @param fn_layerName character, new name for the layer
#' @param x environment, a study
#' @param fileFolder root folder containing at least a folder (which name is gone to be used as layer name), containing one tiff mask for each sample
#' @param orderAgain numeric vector, which mask match which raw data file. Consider that list.files produce a list of files in sttrict alphabetical order
#' @param transposeImage logical, parameter passed to fn_transpose
#' @param layerName character, parameter passed to fn_layerName
#' @param saveToDisk logical, save rater to disk? absolutely for large data sets
#' @param ... not implemented
#' @return an object of class IMC_classification
#' @seealso
#' @examples
#'
#' \dontrun{
#' importTiffMask(x = TEST,
#' fileFolder='c:/somewhere...',
#' orderAgain =c(1,3,2),
#' transposeImage = F,
#' layerName = 'Import',
#' saveToDisk = T ))
#' }
#' @details .importTiffMask: internal
#' @export
#' @docType methods
#' @rdname ImportTiffMask
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


#' @details importTiffMask: method to import tiff masks
#' @export
#' @docType methods
#' @rdname ImportTiffMask
  setGeneric("importTiffMask", function(x,fileFolder=NULL,orderAgain=NULL,transposeImage=T,layerName='Tiff_imported',saveToDisk=T, ...)
    standardGeneric("importTiffMask"))

setMethod('importTiffMask',signature = ('environment'),
          function(x,fileFolder=NULL,orderAgain=NULL,transposeImage=T,layerName='Tiff_imported',saveToDisk=T, ...){


            if (is.null(fileFolder)) {stop(RUNIMC:::mError('specify a folder where to look for .tiff'))}
            if (!dir.exists(fileFolder)) {stop(RUNIMC:::mError('cannot find specified folder'))}
            if (is.null(orderAgain)) {message(RUNIMC:::mWarning('an order has not been specified so files are supposed to be in the correct order'))}

            uids<-x$studyTable$uid

            dirToImport<-list.dirs(fileFolder,full.names = T,recursive = F)
            labelToImport<-list.dirs(fileFolder,full.names = F,recursive = F)
            newClassificationList<-list()

            for (lbl in 1:length(labelToImport)){

              newLabel<-paste(layerName,labelToImport[lbl],sep='_')
              filesToImport<-list.files(dirToImport[[lbl]],full.names = T )

              if (length(uids)!=length(filesToImport)) {stop(RUNIMC:::mError('the number of files in this folder does not match the number of samples for this study'))}
              filesToImport<-filesToImport[orderAgain]

              newClassification<-.importTiffMask(fn_fileList = filesToImport,
                                                 fn_transpose = transposeImage,
                                                 fn_layerName = newLabel)

              for (i in 1:length(newClassification)){
                comparisonRst<-raster::compareRaster(newClassification[[i]],
                                                     x$raster[[i]],
                                                     extent = F,
                                                     rowcol = T,
                                                     crs = T,
                                                     stopiffalse = F,
                                                     res = T)
                if (!comparisonRst){stop(RUNIMC:::mError('raster and masks do not match'))}

                raster::extent(newClassification[[i]])<-raster::extent(x$raster[[i]])

                newClassificationList[[uids[i]]][[newLabel]]<-list()
                newClassificationList[[uids[i]]][[newLabel]]<-newClassification[[i]]
              }

            }



            if (saveToDisk) {
              filePath<-file.path(x$currentAnalysis$folder,
                                  'test/classification')
              rasterFilePath<-checkDir(filePath,'rasters')
              stackFilePath<-checkDir(filePath,'rasterStacks')
            }

            newClassification<-sapply(uids,function(uid){
              if (saveToDisk) {

                TXT_file<-x$studyTable$IMC_text_file[x$studyTable$uid==uid]
                dirPath<-checkDir(rasterFilePath,TXT_file)
                rst<-list()
                for (lbl in names(newClassificationList[[uid]])){
                  filePath<-file.path(dirPath,paste0(lbl,'.grd'))
                  rst[[lbl]]<-raster::writeRaster(x=newClassificationList[[uid]][[lbl]],
                                                  filename = filePath,
                                                  overwrite=T,
                                                  format='raster')
                }


                rstrStk<-IMC_stack(x = rst,
                                   uid = uid,
                                   IMC_text_file = TXT_file,
                                   study = x$studyTable$study[x$studyTable$uid==uid],
                                   sample = x$studyTable$sample[x$studyTable$uid==uid],
                                   replicate = x$studyTable$replicate[x$studyTable$uid==uid],
                                   ROI = x$studyTable$ROI[x$studyTable$uid==uid],
                                   bioGroup = x$studyTable$bioGroup[x$studyTable$uid==uid],
                                   channels = data.frame())

                names(rstrStk)<-names(newClassificationList[[uid]])

                rstrStk<-IMCstackSave(rstrStk,file.path(stackFilePath,paste0(TXT_file,'.stk')))

              } else {
                rstrStk<-IMC_stack(x = newClassificationList[[uid]],
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

