#' Archive
#'
#' Archive is used to save to disk an entire study, an Analysis or a part of it
#'
#' @param x a study (environment) or one of its components
#' @param what which component of a study to archive (not implemented at the moment)
#' @param objectReturn should the function return the object (TRUE) or the file path (FALSE)?
#' @param forceSave each object in a study is time stamped and saving is inhibited if this object has not been modified. forceSave set to TRUE revert this behaviour
#' @param studyTable explicitly provide the study table
#' @return the same object after it has been written to disk or a character string representing the file path
#' @seealso
#' @examples
#' \dontrun{
#' archive(x = MyStudy)
#' }
#' @details archive has specific methods for a study and for its elements
#' @export
#' @docType methods
#' @rdname Archive-methods
setGeneric("archive", function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,studyTable=NULL,...)
  standardGeneric("archive"))

setMethod('archive',signature = ('environment'),
          function(x,what=NULL,objectReturn=F,forceSave=F,studyTable=NULL,...){

            objectList<-ls(x)

            if (any('rootFolder' %in% objectList)){


              if (!forceSave){
                mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
                artnTimeStmp<-attr(x,'artnTimeStmp')
                if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                  if (mdtnTimeStmp==artnTimeStmp){
                    message(mWarning('Archived study is up to date, you might use "forceSave=T"'))
                    return(0)
                  }
                }
              }

              basePath<-paste(x$rootFolder,x$name,sep='/')

              #### study table
              outF<-archive(x = x$studyTable,
                            filePathName = paste(basePath,'st',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$studyTable,'mdtnTimeStmp')<-newTimeStmp
                attr(x$studyTable,'artnTimeStmp')<-newTimeStmp
                attr(x$studyTable,'fileArchive')<-outF
                message(mMessage('Study table archivation... OK'))
              }
              #### channel table
              outF<-archive(x = x$channels,
                            filePathName = paste(basePath,'ch',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$channels,'mdtnTimeStmp')<-newTimeStmp
                attr(x$channels,'artnTimeStmp')<-newTimeStmp
                attr(x$channels,'fileArchive')<-outF
                message(mMessage('Channel table archivation... OK'))
              }
              #### rasters

              outF<-archive(x = x$raster,
                            filePathName = paste(basePath),
                            objectReturn = F,
                            forceSave = forceSave,
                            studyTable=x$studyTable)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$raster,'mdtnTimeStmp')<-newTimeStmp
                attr(x$raster,'artnTimeStmp')<-newTimeStmp
                attr(x$raster,'fileArchive')<-outF
                message(mMessage('Raster archivation... OK'))
              }
              #### current analysis
              outF<-archive(x = x$currentAnalysis,
                            filePathName = paste(basePath,'Analysis',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave,
                            studyTable = x$studyTable)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
                attr(x$currentAnalysis,'artnTimeStmp')<-newTimeStmp
                attr(x$currentAnalysis,'fileArchive')<-outF
                message(mMessage('Current analysis archivation... OK'))
              }

              writePermit<-file.access(paste0(basePath,'/study.xml'), mode=2)
              fileExists<-file.exists(paste0(basePath,'/study.xml'))
              if (writePermit==0 | !fileExists){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x,'mdtnTimeStmp')<-newTimeStmp
                attr(x,'artnTimeStmp')<-newTimeStmp
                attr(x,'fileArchive')<-paste0(basePath,'/study.xml')
                xmlOut<-XMLparseObject(x,'study')
                XML::saveXML(xmlOut,paste0(basePath,'/study.xml'))
              } else {stop(RUNIMC:::mError(paste0('Write permission denied to: ',basePath,'/study.xml')))}

              if (objectReturn){

                return(x)
              }
              return(paste0(basePath,'/study.xml'))
            }

            #### current analysis
            if (any('folder' %in% objectList)){


              if (!forceSave){
                mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
                artnTimeStmp<-attr(x,'artnTimeStmp')
                if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                  if (mdtnTimeStmp==artnTimeStmp){
                    message(mWarning('Archived study is up to date, you might use "forceSave=T"'))
                    return(0)
                  }
                }
              }

              basePath<-x$folder

              #### classification
              outF<-archive(x = x$classification,
                            filePathName = file.path(basePath,'/test/classification'),
                            objectReturn = F,
                            forceSave = forceSave,
                            studyTable = studyTable)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$classification,'mdtnTimeStmp')<-newTimeStmp
                attr(x$classification,'artnTimeStmp')<-newTimeStmp
                attr(x$classification,'fileArchive')<-outF
                message(mMessage('Classification archivation... OK'))
              }
              #### classificationDirectives
              outF<-archive(x = x$classificationDirectives,
                            filePathName = paste(basePath,'cd',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$classificationDirectives,'mdtnTimeStmp')<-newTimeStmp
                attr(x$classificationDirectives,'artnTimeStmp')<-newTimeStmp
                attr(x$classificationDirectives,'fileArchive')<-outF
                message(mMessage('Classification directives archivation... OK'))
              }
              #### classifier
              outF<-archive(x = x$classifier,
                            filePathName = paste(basePath,'cs',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$classifier,'mdtnTimeStmp')<-newTimeStmp
                attr(x$classifier,'artnTimeStmp')<-newTimeStmp
                attr(x$classifier,'fileArchive')<-outF
                message(mMessage('Classifier archivation... OK'))
              }
              #### derivedRasters
              outF<-archive(x = x$derivedRasters,
                            filePathName = basePath,
                            objectReturn = F,
                            forceSave = forceSave,
                            studyTable=parent.env(x)$studyTable)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$derivedRasters,'mdtnTimeStmp')<-newTimeStmp
                attr(x$derivedRasters,'artnTimeStmp')<-newTimeStmp
                attr(x$derivedRasters,'fileArchive')<-outF
                message(mMessage('Derived raster archivation... OK'))
              }
              #### exprs
              outF<-archive(x = x$exprs,
                            filePathName = paste(basePath,'ex',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$exprs,'mdtnTimeStmp')<-newTimeStmp
                attr(x$exprs,'artnTimeStmp')<-newTimeStmp
                attr(x$exprs,'fileArchive')<-outF
                message(mMessage('Expression table archivation... OK'))
              }
              #### extractionDirectives
              outF<-archive(x = x$extractionDirectives,
                            filePathName = paste(basePath,'ed',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$extractionDirectives,'mdtnTimeStmp')<-newTimeStmp
                attr(x$extractionDirectives,'artnTimeStmp')<-newTimeStmp
                attr(x$extractionDirectives,'fileArchive')<-outF
                message(mMessage('Extraction directives archivation... OK'))
              }
              #### filters
              outF<-archive(x = x$filters,
                            filePathName = paste(basePath,'fl',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$filters,'mdtnTimeStmp')<-newTimeStmp
                attr(x$filters,'artnTimeStmp')<-newTimeStmp
                attr(x$filters,'fileArchive')<-outF
                message(mMessage('Filter archivation... OK'))
              }
              #### interpretationMatrix
              outF<-archive(x = x$interpretationMatrix,
                            filePathName = paste(basePath,'im',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$interpretationMatrix,'mdtnTimeStmp')<-newTimeStmp
                attr(x$interpretationMatrix,'artnTimeStmp')<-newTimeStmp
                attr(x$interpretationMatrix,'fileArchive')<-outF
                message(mMessage('Interpretation matrix archivation... OK'))
              }
              #### segmentation
              outF<-archive(x = x$segmentation,
                            filePathName = paste(basePath,'sg',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$segmentation,'mdtnTimeStmp')<-newTimeStmp
                attr(x$segmentation,'artnTimeStmp')<-newTimeStmp
                attr(x$segmentation,'fileArchive')<-outF
                message(mMessage('Segmentation archivation... OK'))
              }
              #### segmentationDirectives
              outF<-archive(x = x$segmentationDirectives,
                            filePathName = paste(basePath,'sd',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave)
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$segmentationDirectives,'mdtnTimeStmp')<-newTimeStmp
                attr(x$segmentationDirectives,'artnTimeStmp')<-newTimeStmp
                attr(x$segmentationDirectives,'fileArchive')<-outF
                message(mMessage('Segmentation directives archivation... OK'))
              }
              #### trainingFeatures
              outF<-archive(x = x$trainingFeatures,
                            filePathName = paste(basePath,'tf',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave,
              )
              if (outF!=-1 & outF!=0){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x$trainingFeatures,'mdtnTimeStmp')<-newTimeStmp
                attr(x$trainingFeatures,'artnTimeStmp')<-newTimeStmp
                attr(x$trainingFeatures,'fileArchive')<-outF
                message(mMessage('Training features archivation... OK'))
              }

              writePermit<-file.access(paste0(basePath,'/analysis.xml'), mode=2)
              fileExists<-file.exists(paste0(basePath,'/analysis.xml'))
              if (writePermit==0 | !fileExists){
                newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                attr(x,'mdtnTimeStmp')<-newTimeStmp
                attr(x,'artnTimeStmp')<-newTimeStmp
                attr(x,'fileArchive')<-paste0(basePath,'/analysis.xml')
                xmlOut<-XMLparseObject(x,'analysis')
                XML::saveXML(xmlOut,paste0(basePath,'/analysis.xml'))
              } else {stop(RUNIMC:::mError(paste0('Write permission denied to: ',basePath,'/analysis.xml')))}

              if (objectReturn){

                return(x)
              }
              return(paste0(basePath,'/analysis.xml'))

            }


          })

#*** archive:imc_studyTable ---------------------------------------------------

setMethod('archive',signature = 'IMC_StudyTable',
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){

            if (is.null(filePathName)) stop(mError('Provide file name'))
            if (!endsWith(filePathName,'.IMC_StudyTable')) filePathName<-paste0(filePathName,'.IMC_StudyTable')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Study table file is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            fileOK<-try(utils::write.table(x,filePathName,row.names = F,quote = F,sep = '\t'))

            if (exists('fileOK')){
              if (!is.null(fileOK)){
                if (inherits(fileOK,what = 'try-error')){
                  message(mError('Study table file archiviation failed'))
                  return(-1)
                }
              }
            }
            if (objectReturn){
              return(x)
            }
            return(filePathName)
          })


#*** archive:imc_channeltable ---------------------------------------------------

setMethod('archive',signature = 'IMC_ChannelTable',
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){

            if (is.null(filePathName)) stop(mError('Provide file name'))
            if (!endsWith(filePathName,'.IMC_ChannelTable')) filePathName<-paste0(filePathName,'.IMC_ChannelTable')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Channel table file is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            fileOK<-try(utils::write.table(x,filePathName,row.names = F,quote = F,sep = '\t'))

            if (exists('fileOK')){
              if (!is.null(fileOK)){
                if (inherits(fileOK,what = 'try-error')){
                  message(mError('Channel table file archiviation failed'))
                  return(-1)
                }
              }
            }
            if (objectReturn){
              return(x)
            }
            return(filePathName)
          })


#*** archive:imc_RsCollection ---------------------------------------------------

setMethod('archive',signature = c('IMC_RsCollection'),
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,studyTable=NULL,...){


            if (is.null(filePathName)) stop(mError('Provide file name'))
            # if (!endsWith(filePathName,'.txt')) filePathName<-paste0(filePathName,'.txt')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('RasterStack collection is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            dir.create(filePathName,showWarnings = F)

            rstrStk<-sapply(names(x),function(smp){

              if (is.null(studyTable)){
                studyTable<-data.frame(uid=smp,IMC_text_file=smp,study='NO_STUDY',sample='NO_sample',replicte='NO_replicate',ROI='NO_ROI',bioGroup='NO_BIOGROUP',stringsAsFactors = F)
              }
              newDir<-checkDir(filePathName,'rasterStacks')
              newDir<-checkDir(filePathName,'rasters')
              newDir<-checkDir(paste(filePathName,'rasters',sep='/'),studyTable$IMC_text_file[studyTable$uid==smp])


              rstFile<-lapply(names(x[[smp]]),function(chnl){

                fileObjective<-paste0(newDir,'/',chnl,'.nc')

                if (raster::fromDisk(x[[smp]][[chnl]])){
                  temp_raster<-raster::readAll(x[[smp]][[chnl]])
                  unlink(fileObjective,force = T)
                  raster::writeRaster(x = temp_raster,
                                      filename = fileObjective,
                                      overwrite=T,
                                      format='CDF')} else{
                                        raster::writeRaster(x = x[[smp]][[chnl]],
                                                            filename = fileObjective,
                                                            overwrite=T,
                                                            format='CDF')
                                      }
                return(fileObjective)
              })


              stackDetails<-studyTable[studyTable$uid==smp,]
              rstrStk<-IMC_stack(x = rstFile,
                                 uid = smp,
                                 IMC_text_file = stackDetails$IMC_text_file,
                                 study = stackDetails$study,
                                 sample = stackDetails$sample,
                                 replicate = stackDetails$replicate,
                                 ROI = stackDetails$ROI,
                                 bioGroup = stackDetails$bioGroup,
                                 channels = data.frame(RcolumnNames = names(x[[smp]])))


              IMCstackSave(x = rstrStk,
                           filename = paste0(filePathName,'/rasterStacks/',
                                             studyTable$IMC_text_file[studyTable$uid==smp],'.stk'))

              return(rstrStk)
            },USE.NAMES = T,simplify = F)


            rstrStk<-new('IMC_RsCollection',rstrStk)


            if (objectReturn){
              return(rstrStk)
            }
            return(paste0(filePathName,'/rasterStacks'))
          })

#*** archive:imc_classification ---------------------------------------------------

setMethod('archive',signature = c('IMC_Classification'),
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,studyTable=NULL,...){


            if (is.null(filePathName)) stop(mError('Provide file name'))
            # if (!endsWith(filePathName,'.txt')) filePathName<-paste0(filePathName,'.txt')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Classification is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            dir.create(filePathName,showWarnings = F)

            rstrStk<-sapply(names(x),function(smp){

              if (is.null(studyTable)){
                studyTable<-data.frame(uid=smp,IMC_text_file=smp,study='NO_STUDY',sample='NO_sample',replicte='NO_replicate',ROI='NO_ROI',bioGroup='NO_BIOGROUP',stringsAsFactors = F)
              }
              newDir<-checkDir(filePathName,'rasterStacks')
              newDir<-checkDir(filePathName,'rasters')
              newDir<-checkDir(paste(filePathName,'rasters',sep='/'),studyTable$IMC_text_file[studyTable$uid==smp])


              rstFile<-lapply(names(x[[smp]]),function(chnl){

                fileObjective<-paste0(newDir,'/',chnl,'.grd')

                if (raster::fromDisk(x[[smp]][[chnl]])){
                  temp_raster<-raster::readAll(x[[smp]][[chnl]])
                  unlink(fileObjective,force = T)
                  raster::writeRaster(x = temp_raster,
                                      filename = fileObjective,
                                      overwrite=T,
                                      format='raster')} else{
                                        raster::writeRaster(x = x[[smp]][[chnl]],
                                                            filename = fileObjective,
                                                            overwrite=T,
                                                            format='raster')
                                      }
                return(fileObjective)
              })


              stackDetails<-studyTable[studyTable$uid==smp,]
              rstrStk<-IMC_stack(x = rstFile,
                                 uid = smp,
                                 IMC_text_file = stackDetails$IMC_text_file,
                                 study = stackDetails$study,
                                 sample = stackDetails$sample,
                                 replicate = stackDetails$replicate,
                                 ROI = stackDetails$ROI,
                                 bioGroup = stackDetails$bioGroup,
                                 channels = data.frame(RcolumnNames = names(x[[smp]])))


              IMCstackSave(x = rstrStk,
                           filename = paste0(filePathName,'/rasterStacks/',
                                             studyTable$IMC_text_file[studyTable$uid==smp],'.stk'))

              return(rstrStk)
            },USE.NAMES = T,simplify = F)


            rstrStk<-new('IMC_Classification',rstrStk)


            if (objectReturn){
              return(rstrStk)
            }
            return(paste0(filePathName,'/rasterStacks'))
          })


#*** archive:imc_filterframe ---------------------------------------------------

setMethod('archive',signature = 'IMC_FilterFrame',
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){

            if (is.null(filePathName)) stop(mError('Provide file name'))
            if (!endsWith(filePathName,'.IMC_FilterFrame')) filePathName<-paste0(filePathName,'.IMC_FilterFrame')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Filter definition file is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            fileOK<-try(saveRDS(x,filePathName))

            if (exists('fileOK')){
              if (!is.null(fileOK)){
                if (inherits(fileOK,what = 'try-error')){
                  message(mError('Filter definition file archiviation failed'))
                  return(-1)
                }
              }
            }
            if (objectReturn){
              return(x)
            }
            return(filePathName)
          })


#*** archive:imc_extractiondirectives ---------------------------------------------------

setMethod('archive',signature = c('IMC_ExtractionDirectives'),
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){


            if (is.null(filePathName)) stop(mError('Provide file name'))
            if (!endsWith(filePathName,'.IMC_ExtractionDirectives')) filePathName<-paste0(filePathName,'.IMC_ExtractionDirectives')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Extraction directives file is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            fileOK<-try(saveRDS(x,filePathName))

            if (exists('fileOK')){
              if (!is.null(fileOK)){
                if (inherits(fileOK,what = 'try-error')){
                  message(mError('Extraction directives file archiviation failed'))
                  return(-1)
                }
              }
            }
            if (objectReturn){
              return(x)
            }
            return(filePathName)
          })

#*** archive:imc_classificationdirectives ---------------------------------------------------

setMethod('archive',signature = c('IMC_ClassificationDirectives'),
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){


            if (is.null(filePathName)) stop(mError('Provide file name'))
            if (!endsWith(filePathName,'.IMC_ClassificationDirectives')) filePathName<-paste0(filePathName,'.IMC_ClassificationDirectives')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Classification directives file is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            fileOK<-try(saveRDS(x,filePathName))

            if (exists('fileOK')){
              if (!is.null(fileOK)){
                if (inherits(fileOK,what = 'try-error')){
                  message(mError('Classification directives file archiviation failed'))
                  return(-1)
                }
              }
            }
            if (objectReturn){
              return(x)
            }
            return(filePathName)
          })

#*** archive:imc_segmantationdirectives ---------------------------------------------------

setMethod('archive',signature = c('IMC_SegmentationDirectives'),
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){


            if (is.null(filePathName)) stop(mError('Provide file name'))
            if (!endsWith(filePathName,'.IMC_SegmentationDirectives')) filePathName<-paste0(filePathName,'.IMC_SegmentationDirectives')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Segmentation directives file is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            fileOK<-try(saveRDS(x,filePathName))

            if (exists('fileOK')){
              if (!is.null(fileOK)){
                if (inherits(fileOK,what = 'try-error')){
                  message(mError('Segmentation directives file archiviation failed'))
                  return(-1)
                }
              }
            }
            if (objectReturn){
              return(x)
            }
            return(filePathName)
          })

#*** archive:imc_InterpretationMatrix ---------------------------------------------------

setMethod('archive',signature = c('IMC_InterpretationMatrix'),
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){


            if (is.null(filePathName)) stop(mError('Provide file name'))
            if (!endsWith(filePathName,'.IMC_InterpretationMatrix')) filePathName<-paste0(filePathName,'.IMC_InterpretationMatrix')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Interpretation matrix file is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            fileOK<-try(saveRDS(x,filePathName))

            if (exists('fileOK')){
              if (!is.null(fileOK)){
                if (inherits(fileOK,what = 'try-error')){
                  message(mError('Interpretation matrix file archiviation failed'))
                  return(-1)
                }
              }
            }
            if (objectReturn){
              return(x)
            }
            return(filePathName)
          })

#*** archive:imc_trainingfeatures ---------------------------------------------------

setMethod('archive',signature = c('IMC_TrainingFeatures'),
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){


            if (is.null(filePathName)) stop(mError('Provide file name'))
            if (!endsWith(filePathName,'IMC_TrainingFeatures')) filePathName<-paste0(filePathName,'.IMC_TrainingFeatures')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Training features file is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            dir.create(filePathName,showWarnings = F)
            fileOK<-try(utils::write.table(x$value,paste(filePathName,'value.txt',sep='/'),row.names = F,quote = F,sep = '\t'))

            if (exists('fileOK')){
              if (!is.null(fileOK)){
                if (inherits(fileOK,what = 'try-error')){
                  message(mError('Training features VALUE file archiviation failed'))
                  return(-1)
                }
              }
            }

            fileOK<-try(utils::write.table(x$geometry,paste(filePathName,'geometry.txt',sep='/'),row.names = F,quote = F,sep = '\t'))

            if (exists('fileOK')){
              if (!is.null(fileOK)){
                if (inherits(fileOK,what = 'try-error')){
                  message(mError('Training features GEOMETRY file archiviation failed'))
                  return(-1)
                }
              }
            }


            if (objectReturn){
              return(x)
            }
            return(filePathName)
          })


#*** archive:imc_segmentationList ---------------------------------------------------

setMethod('archive',signature = c('IMC_SegmentationList'),
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){


            if (is.null(filePathName)) stop(mError('Provide file name'))
            if (!endsWith(filePathName,'.IMC_SegmentationList')) filePathName<-paste0(filePathName,'.IMC_SegmentationList')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Segmentation file is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            fileOK<-try(saveRDS(x,filePathName))

            if (exists('fileOK')){
              if (!is.null(fileOK)){
                if (inherits(fileOK,what = 'try-error')){
                  message(mError('Segmentation file archiviation failed'))
                  return(-1)
                }
              }
            }
            if (objectReturn){
              return(x)
            }
            return(filePathName)
          })


#*** archive:imc_expressionmatrix ---------------------------------------------------

setMethod('archive',signature = c('sf','missing'),
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){


            if (is.null(filePathName)) stop(mError('Provide file name'))
            if (!endsWith(filePathName,'.sqlite')) filePathName<-paste0(filePathName,'.sqlite')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Expression file is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            fileOK<-try( sf::st_write(x,filePathName,append=F,quiet=T))

            if (exists('fileOK')){
              if (!is.null(fileOK)){
                if (inherits(fileOK,what = 'try-error')){
                  message(mError('Expression file archiviation failed'))
                  return(-1)
                }
              }
            }
            if (objectReturn){
              return(x)
            }
            return(filePathName)
          })

#*** archive:imc_classifier ---------------------------------------------------

setMethod('archive',signature = c('IMC_Classifier'),
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){


            if (is.null(filePathName)) stop(mError('Provide file name'))
            if (!endsWith(filePathName,'.IMC_Classifier')) filePathName<-paste0(filePathName,'.IMC_Classifier')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Classifier file is up to date, you might use "forceSave=T"'))
                  return(0)
                }
              }
            }

            fileOK<-try(saveRDS(x,filePathName))

            if (exists('fileOK')){
              if (!is.null(fileOK)){
                if (inherits(fileOK,what = 'try-error')){
                  message(mError('Classifier file archiviation failed'))
                  return(-1)
                }
              }
            }
            if (objectReturn){
              return(x)
            }
            return(filePathName)
          })

#*** archive:NULL---------------------------------------------------

setMethod('archive',signature = c('NULL'),
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){

            object<-deparse(substitute(x))
            message(mWarning(paste0(object,' is NULL, nothing to archive')))
            return(-1)

          })

