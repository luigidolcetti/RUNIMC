# retrieve<-function(fn_study=NULL,
#                    fn_analysis=NULL,
#                    fn_what=NULL,
#                    fn_path=NULL,
#                    fn_file=NULL){
#
#   if (!is.null(fn_study) & !is.null(fn_path) & !is.null(fn_file)) message(RUNIMC:::mWarning('Value in this study will be replaced'))
#   if (!is.null(fn_study) & is.null(fn_what)) message(RUNIMC:::mWarning('All the values in this study will be retrieved'))
#   # if (!is.null(fn_study) & !is.null(fn_path) & !is.null(fn_file)) stop(RUNIMC:::mError('Please either specify a study and optionally the information to retrieve or a specific path and file'))
#   if (!is.null(fn_path) & is.null(fn_file)) stop(RUNIMC:::mError('Please, specify a file to retrieve'))
#   if (is.null(fn_path) & !is.null(fn_file)) stop(RUNIMC:::mError('Please, specify a path where to search for this file'))
#
#   if (!is.null(fn_path)){
#     if (!dir.exists(fn_path)) stop(RUNIMC:::mError(paste0('Unable to find: ',fn_path)))}
#
#
#   if (!is.null(fn_analysis) & !is.null(fn_study)){
#     if (!any(fn_study %in% fn_study$analysis)) stop(RUNIMC:::mError(paste0('Unable to find ',fn_analysis,' analysis in this study')))
#   }
#   if (is.null(fn_path) & !is.null(fn_study) & !is.null(fn_analysis)){
#     fn_path<-file.path(fn_study$rootFolder,fn_study$name,'analysis',fn_analysis)
#   }
#
#   if (!is.null(fn_file)) {
#     fileExtension<-unlist(strsplit(fn_file,'\\.'))
#     if (!length(fileExtension)>1) {stop(RUNIMC:::mError('File without extension cannot decide were to store this information'))}
#     fileExtension<-fileExtension[length(fileExtension)]
#     if (!any(fileExtension %in% c('IMC_ChannelTable',
#                                   'IMC_StudyTable',
#                                   'xml',
#                                   'IMC_ClassificationDirectives',
#                                   'IMC_Classifier',
#                                   'sqlite',
#                                   'IMC_FilterFrame',
#                                   'IMC_InterpretationMatrix',
#                                   'IMC_SegmentationDirectives',
#                                   'IMC_SegmentationList',
#                                   'IMC_ExtractionDirectives',
#                                   'IMC_Classification'))) {stop(RUNIMC:::mError('Unknown file extension'))}
#     fullPath<-file.path(fn_path,fn_file)
#     switch(fileExtension,
#            IMC_ChannelTable={
#              retrieve.channelTable(fn_study,fn_analysis,fn_what,fn_path,fn_file)
#              if (is.null(fn_study)) return(fileContent) else fn_study$channels<-fileContent},
#            IMC_StudyTable={
#              retrieve.channelTable(fn_study,fn_analysis,fn_what,fn_path,fn_file)
#              if (is.null(fn_study)) return(fileContent) else fn_study$channels<-fileContent},
#
#
#     )
#
#   }
#
# }


retrieve.channelTable<-function(fn_file=NULL,
                                fn_timeStamp=T){
  fileContent<-try(utils::read.table(fn_file,header = T,as.is = T,sep = '\t'))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

retrieve.studyTable<-function(fn_file=NULL,
                              fn_timeStamp=T){
  fileContent<-try(utils::read.table(fn_file,header = T,as.is = T,sep = '\t'))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

# retrieve.classificationDirectives<-function(fn_file=NULL,
#                                             fn_timeStamp=T){
#
#   fileContent<-try(readRDS(fn_file))
#   if (exists('fileContent')){
#     if (!is.null(fileContent)){
#       if (inherits(fileContent,what = 'try-error')){
#         stop(mError('Cannot open file archive'))
#       }
#     }
#   }
#   if (fn_timeStamp){
#     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#     fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
#     attr(fileContent,'fileArchive')<-fn_file
#   }
#   return(fileContent)
# }

retrieve.classifier<-function(fn_file=NULL,
                              fn_timeStamp=T){
  fileContent<-try(readRDS(fn_file))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

retrieve.classificationDirectives<-function(fn_file=NULL,
                                            fn_timeStamp=T){
  fileContent<-try(readRDS(fn_file))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

retrieve.expressionMatrix<-function(fn_file=NULL,
                                    fn_timeStamp=T){
  browser
  fileContent<-try(sf::st_read(fn_file,as_tibble=F,geometry_column='GEOMETRY',quiet=T))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

retrieve.filterFrame<-function(fn_file=NULL,
                               fn_timeStamp=T){
  fileContent<-try(readRDS(fn_file))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

retrieve.interpretationMatrix<-function(fn_file=NULL,
                                        fn_timeStamp=T){

  fileContent<-try(readRDS(fn_file))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

retrieve.filterFrame<-function(fn_file=NULL,
                               fn_timeStamp=T){
  fileContent<-try(readRDS(fn_file))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

retrieve.segmentationDirectives<-function(fn_file=NULL,
                                          fn_timeStamp=T){
  fileContent<-try(readRDS(fn_file))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

retrieve.segmentationList<-function(fn_file=NULL,
                                    fn_timeStamp=T){
  fileContent<-try(readRDS(fn_file))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

retrieve.extractionDirectives<-function(fn_file=NULL,
                                        fn_timeStamp=T){
  fileContent<-try(readRDS(fn_file))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

retrieve.classification<-function(fn_file=NULL,
                                  fn_timeStamp=T){
  fileContent<-try(readRDS(fn_file))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

retrieve.trainingFeatures<-function(fn_file=NULL,
                                  fn_timeStamp=T){

  fileGeometry<-try(utils::read.table(file.path(fn_file,'geometry.txt'),header = T,as.is = T,sep = '\t'))
  fileValue<-try(utils::read.table(file.path(fn_file,'value.txt'),header = T,as.is = T,sep = '\t'))

  if (exists('fileGeometry') & exists('fileValue')){
    if (!is.null(fileGeometry) & !is.null(fileValue)){
      if (inherits(fileGeometry,what = 'try-error') | inherits(fileValue,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }

  newTrainingFeatures<-list(value=fileValue,geometry=fileGeometry)
  newTrainingFeatures<-new('IMC_TrainingFeatures',newTrainingFeatures)

  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    newTrainingFeatures<-RUNIMC:::timeStampObject(newTrainingFeatures,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(newTrainingFeatures,'fileArchive')<-fn_file
  }
  return(newTrainingFeatures)
}

retrieve.RsCollection<-function(fn_file=NULL,
                                  fn_timeStamp=T){
  rstList<-list.files(fn_file,full.names = T)
  fileContent<-lapply(rstList,function(x){
    rst<-RUNIMC:::IMCstackOpen(x)
  })
  names(fileContent)<-lapply(fileContent,function(x)x@uid)
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  if (fn_timeStamp){
    newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
    fileContent<-RUNIMC:::timeStampObject(fileContent,newTimeStmp,newTimeStmp,newTimeStmp)
    attr(fileContent,'fileArchive')<-fn_file
  }
  return(fileContent)
}

retrieve.xml<-function(fn_file=NULL,
                       fn_timeStamp=T){
  fileContent<-try(XML::xmlParse(fn_file))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  rootNode<-XML::xmlRoot(fileContent)
  childrenNode<-XML::xmlChildren(rootNode)
  chldNdNames<-names(childrenNode)
  if (any('currentAnalysis' %in% chldNdNames) & !any('exprs' %in% chldNdNames)){
    newStudy<-new.env()
    attr(newStudy,'crtnTimeStmp')<-XML::xmlGetAttr(rootNode,'crtnTimeStmp')
    attr(newStudy,'mdtnTimeStmp')<-XML::xmlGetAttr(rootNode,'mdtnTimeStmp')
    attr(newStudy,'artnTimeStmp')<-XML::xmlGetAttr(rootNode,'artnTimeStmp')
    attr(newStudy,'fileArchive')<-XML::xmlGetAttr(rootNode,'fileArchive')
    newStudy$name<-as.character(XML::xmlValue(childrenNode$name))
    newStudy$rootFolder<-as.character(XML::xmlValue(childrenNode$rootFolder))
    newStudy$rawDataFolder<-as.character(XML::xmlValue(childrenNode$rawDataFolder))
    newStudy$whichColumns<-as.character(XML::xmlValue(childrenNode$whichColumns))
    newStudy$analysis<-unlist(strsplit(x = XML::xmlValue(childrenNode$analysis),split = ',',fixed = T),recursive = T)
    #studyTable
    targetFile<-XML::xmlValue(childrenNode$studyTable,'fileArchive')
    if (file.exists(targetFile)){
    newStudy$studyTable<-retrieve(x = NULL,fn_path = "",fn_file = targetFile)
    attr(newStudy$studyTable,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$studyTable,'crtnTimeStmp')
    attr(newStudy$studyTable,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$studyTable,'mdtnTimeStmp')
    attr(newStudy$studyTable,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$studyTable,'artnTimeStmp')
    attr(newStudy$studyTable,'fileArchive')<-XML::xmlGetAttr(childrenNode$studyTable,'fileArchive')} else {newStudy$studyTable<-NULL}
    #channelTable
    targetFile<-XML::xmlValue(childrenNode$channels,'fileArchive')
    if (file.exists(targetFile)){
      newStudy$channels<-retrieve(x = NULL,fn_path = "",fn_file = targetFile)
      attr(newStudy$studyTable,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$channels,'crtnTimeStmp')
      attr(newStudy$studyTable,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$channels,'mdtnTimeStmp')
      attr(newStudy$studyTable,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$channels,'artnTimeStmp')
      attr(newStudy$studyTable,'fileArchive')<-XML::xmlGetAttr(childrenNode$channels,'fileArchive')} else {newStudy$channels<-NULL}
    #rasters
    targetFile<-XML::xmlValue(childrenNode$raster,'fileArchive')
    if (dir.exists(targetFile)){
      newStudy$raster<-retrieve(x = NULL,fn_path = targetFile,fn_file = NULL)
      attr(newStudy$studyTable,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$raster,'crtnTimeStmp')
      attr(newStudy$studyTable,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$raster,'mdtnTimeStmp')
      attr(newStudy$studyTable,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$raster,'artnTimeStmp')
      attr(newStudy$studyTable,'fileArchive')<-XML::xmlGetAttr(childrenNode$raster,'fileArchive')} else {newStudy$raster<-NULL}
    #currentAnalysis

    targetFile<-XML::xmlValue(childrenNode$currentAnalysis,'fileArchive')
    if (file.exists(targetFile)){
      newStudy$currentAnalysis<-retrieve(x = NULL,fn_path = '',fn_file = targetFile)
      attr(newStudy$studyTable,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$raster,'crtnTimeStmp')
      attr(newStudy$studyTable,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$raster,'mdtnTimeStmp')
      attr(newStudy$studyTable,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$raster,'artnTimeStmp')
      attr(newStudy$studyTable,'fileArchive')<-XML::xmlGetAttr(childrenNode$raster,'fileArchive')} else {newStudy$raster<-NULL}

    return(newStudy)

  }
  #parse Analysis
  if (!any('currentAnalysis' %in% chldNdNames) & any('exprs' %in% chldNdNames)){

    newAnal<-new.env()
    attr(newAnal,'crtnTimeStmp')<-XML::xmlGetAttr(rootNode,'crtnTimeStmp')
    attr(newAnal,'mdtnTimeStmp')<-XML::xmlGetAttr(rootNode,'mdtnTimeStmp')
    attr(newAnal,'artnTimeStmp')<-XML::xmlGetAttr(rootNode,'artnTimeStmp')
    attr(newAnal,'fileArchive')<-XML::xmlGetAttr(rootNode,'fileArchive')
    newAnal$folder<-as.character(XML::xmlValue(childrenNode$folder))
    newAnal$name<-as.character(XML::xmlValue(childrenNode$name))

    #classification
    targetFile<-XML::xmlValue(childrenNode$classification,'fileArchive')
    if (file.exists(targetFile)){
      newAnal$classification<-retrieve(x = NULL,fn_path = "",fn_file = targetFile)
      attr(newAnal$classification,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$classification,'crtnTimeStmp')
      attr(newAnal$classification,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$classification,'mdtnTimeStmp')
      attr(newAnal$classification,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$classification,'artnTimeStmp')
      attr(newAnal$classification,'fileArchive')<-XML::xmlGetAttr(childrenNode$classification,'fileArchive')} else {newAnal$classification<-NULL}
    #classificationDirectives
    targetFile<-XML::xmlValue(childrenNode$classificationDirectives,'fileArchive')
    if (file.exists(targetFile)){
      newAnal$classificationDirectives <-retrieve(x = NULL,fn_path = "",fn_file = targetFile)
      attr(newAnal$classificationDirectives,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$classificationDirectives,'crtnTimeStmp')
      attr(newAnal$classificationDirectives,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$classificationDirectives,'mdtnTimeStmp')
      attr(newAnal$classificationDirectives,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$classificationDirectives,'artnTimeStmp')
      attr(newAnal$classificationDirectives,'fileArchive')<-XML::xmlGetAttr(childrenNode$classificationDirectives,'fileArchive')} else {newAnal$classificationDirectives<-NULL}
    #classifier
    targetFile<-XML::xmlValue(childrenNode$classifier,'fileArchive')
    if (file.exists(targetFile)){
      newAnal$classifier<-retrieve(x = NULL,fn_path = "",fn_file = targetFile)
      attr(newAnal$classifier,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$classifier,'crtnTimeStmp')
      attr(newAnal$classifier,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$classifier,'mdtnTimeStmp')
      attr(newAnal$classifier,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$classifier,'artnTimeStmp')
      attr(newAnal$classifier,'fileArchive')<-XML::xmlGetAttr(childrenNode$classifier,'fileArchive')} else {newAnal$classifier<-NULL}
    #exprs
    targetFile<-XML::xmlValue(childrenNode$exprs,'fileArchive')
    if (file.exists(targetFile)){

      newAnal$exprs<-retrieve(x = NULL,fn_path = "",fn_file = targetFile)
      attr(newAnal$exprs,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$exprs,'crtnTimeStmp')
      attr(newAnal$exprs,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$exprs,'mdtnTimeStmp')
      attr(newAnal$exprs,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$exprs,'artnTimeStmp')
      attr(newAnal$exprs,'fileArchive')<-XML::xmlGetAttr(childrenNode$exprs,'fileArchive')} else {newAnal$exprs<-NULL}
    #extractionDirectives
    targetFile<-XML::xmlValue(childrenNode$extractionDirectives,'fileArchive')
    if (file.exists(targetFile)){
      newAnal$extractionDirectives<-retrieve(x = NULL,fn_path = "",fn_file = targetFile)
      attr(newAnal$extractionDirectives,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$extractionDirectives,'crtnTimeStmp')
      attr(newAnal$extractionDirectives,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$extractionDirectives,'mdtnTimeStmp')
      attr(newAnal$extractionDirectives,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$extractionDirectives,'artnTimeStmp')
      attr(newAnal$extractionDirectives,'fileArchive')<-XML::xmlGetAttr(childrenNode$extractionDirectives,'fileArchive')} else {newAnal$extractionDirectives<-NULL}
    #filters
    targetFile<-XML::xmlValue(childrenNode$filters,'fileArchive')
    if (file.exists(targetFile)){
      newAnal$filters<-retrieve(x = NULL,fn_path = "",fn_file = targetFile)
      attr(newAnal$filters,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$filters,'crtnTimeStmp')
      attr(newAnal$filters,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$filters,'mdtnTimeStmp')
      attr(newAnal$filters,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$filters,'artnTimeStmp')
      attr(newAnal$filters,'fileArchive')<-XML::xmlGetAttr(childrenNode$filters,'fileArchive')} else {newAnal$filters<-NULL}
    #interpretationMatrix
    targetFile<-XML::xmlValue(childrenNode$interpretationMatrix,'fileArchive')
    if (file.exists(targetFile)){
      newAnal$interpretationMatrix<-retrieve(x = NULL,fn_path = "",fn_file = targetFile)
      attr(newAnal$interpretationMatrix,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$interpretationMatrix,'crtnTimeStmp')
      attr(newAnal$interpretationMatrix,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$interpretationMatrix,'mdtnTimeStmp')
      attr(newAnal$interpretationMatrix,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$interpretationMatrix,'artnTimeStmp')
      attr(newAnal$interpretationMatrix,'fileArchive')<-XML::xmlGetAttr(childrenNode$interpretationMatrix,'fileArchive')} else {newAnal$interpretationMatrix<-NULL}
    #segmentation
    targetFile<-XML::xmlValue(childrenNode$segmentation,'fileArchive')
    if (file.exists(targetFile)){
      newAnal$segmentation<-retrieve(x = NULL,fn_path = "",fn_file = targetFile)
      attr(newAnal$segmentation,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$segmentation,'crtnTimeStmp')
      attr(newAnal$segmentation,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$segmentation,'mdtnTimeStmp')
      attr(newAnal$segmentation,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$segmentation,'artnTimeStmp')
      attr(newAnal$segmentation,'fileArchive')<-XML::xmlGetAttr(childrenNode$segmentation,'fileArchive')} else {newAnal$segmentation<-NULL}
    #segmentationDirectives
    targetFile<-XML::xmlValue(childrenNode$segmentationDirectives,'fileArchive')
    if (file.exists(targetFile)){
      newAnal$segmentationDirectives<-retrieve(x = NULL,fn_path = "",fn_file = targetFile)
      attr(newAnal$segmentationDirectives,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$segmentationDirectives,'crtnTimeStmp')
      attr(newAnal$segmentationDirectives,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$segmentationDirectives,'mdtnTimeStmp')
      attr(newAnal$segmentationDirectives,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$segmentationDirectives,'artnTimeStmp')
      attr(newAnal$segmentationDirectives,'fileArchive')<-XML::xmlGetAttr(childrenNode$segmentationDirectives,'fileArchive')} else {newAnal$segmentationDirectives<-NULL}
    #trainingFeatures
    targetFile<-XML::xmlValue(childrenNode$trainingFeatures,'fileArchive')
    if (file.exists(targetFile)){
      newAnal$trainingFeatures<-retrieve(x = NULL,fn_path = "",fn_file = targetFile)
      attr(newAnal$trainingFeatures,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$trainingFeatures,'crtnTimeStmp')
      attr(newAnal$trainingFeatures,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$trainingFeatures,'mdtnTimeStmp')
      attr(newAnal$trainingFeatures,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$trainingFeatures,'artnTimeStmp')
      attr(newAnal$trainingFeatures,'fileArchive')<-XML::xmlGetAttr(childrenNode$trainingFeatures,'fileArchive')} else {newAnal$trainingFeatures<-NULL}
    #derivedRasters
    targetFile<-XML::xmlValue(childrenNode$derivedRasters,'derivedRasters')
    if (dir.exists(targetFile)){
      newAnal$derivedRasters<-retrieve(x = NULL,fn_path = targetFile,fn_file = NULL)
      attr(newAnal$derivedRasters,'crtnTimeStmp')<-XML::xmlGetAttr(childrenNode$derivedRasters,'crtnTimeStmp')
      attr(newAnal$derivedRasters,'mdtnTimeStmp')<-XML::xmlGetAttr(childrenNode$derivedRasters,'mdtnTimeStmp')
      attr(newAnal$derivedRasters,'artnTimeStmp')<-XML::xmlGetAttr(childrenNode$derivedRasters,'artnTimeStmp')
      attr(newAnal$derivedRasters,'fileArchive')<-XML::xmlGetAttr(childrenNode$derivedRasters,'fileArchive')} else {newAnal$derivedRasters<-NULL}


    return(newAnal)


  }

  return(fileContent)
}
