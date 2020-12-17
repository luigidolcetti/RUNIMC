retrieve<-function(fn_study=NULL,
                   fn_analysis=NULL,
                   fn_what=NULL,
                   fn_path=NULL,
                   fn_file=NULL){
  browser()
  if (!is.null(fn_study) & !is.null(fn_path) & !is.null(fn_file)) message(RUNIMC:::mWarning('Value in this study will be replaced'))
  if (!is.null(fn_study) & is.null(fn_what)) message(RUNIMC:::mWarning('All the values in this study will be retrieved'))
  # if (!is.null(fn_study) & !is.null(fn_path) & !is.null(fn_file)) stop(RUNIMC:::mError('Please either specify a study and optionally the information to retrieve or a specific path and file'))
  if (!is.null(fn_path) & is.null(fn_file)) stop(RUNIMC:::mError('Please, specify a file to retrieve'))
  if (is.null(fn_path) & !is.null(fn_file)) stop(RUNIMC:::mError('Please, specify a path where to search for this file'))

  if (!is.null(fn_path)){
    if (!dir.exists(fn_path)) stop(RUNIMC:::mError(paste0('Unable to find: ',fn_path)))}


  if (!is.null(fn_analysis) & !is.null(fn_study)){
    if (!any(fn_study %in% fn_study$analysis)) stop(RUNIMC:::mError(paste0('Unable to find ',fn_analysis,' analysis in this study')))
  }
  if (is.null(fn_path) & !is.null(fn_study) & !is.null(fn_analysis)){
    fn_path<-file.path(fn_study$rootFolder,fn_study$name,'analysis',fn_analysis)
  }

  if (!is.null(fn_file)) {
    fileExtension<-unlist(strsplit(fn_file,'\\.'))
    if (!length(fileExtension)>1) {stop(RUNIMC:::mError('File without extension cannot decide were to store this information'))}
    fileExtension<-fileExtension[length(fileExtension)]
    if (!any(fileExtension %in% c('IMC_ChannelTable',
                                  'IMC_StudyTable',
                                  'xml',
                                  'IMC_ClassificationDirectives',
                                  'IMC_Classifier',
                                  'sqlite',
                                  'IMC_FilterFrame',
                                  'IMC_InterpretationMatrix',
                                  'IMC_SegmentationDirectives',
                                  'IMC_SegmentationList',
                                  'IMC_ExtractionDirectives',
                                  'IMC_Classification'))) {stop(RUNIMC:::mError('Unknown file extension'))}
    fullPath<-file.path(fn_path,fn_file)
    switch(fileExtension,
           IMC_ChannelTable={
             retrieve.channelTable(fn_study,fn_analysis,fn_what,fn_path,fn_file)
             if (is.null(fn_study)) return(fileContent) else fn_study$channels<-fileContent},
           IMC_StudyTable={
             retrieve.channelTable(fn_study,fn_analysis,fn_what,fn_path,fn_file)
             if (is.null(fn_study)) return(fileContent) else fn_study$channels<-fileContent},


    )

  }

}


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

retrieve.classificationDirectives<-function(fn_file=NULL,
                                            fn_timeStamp=T){
  fileContent<-try(dget(fn_file, keep.source = FALSE))
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
  fileContent<-try(dget(file, keep.source = FALSE))
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
  fileContent<-try(dget(fn_file, keep.source = FALSE))
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
  fileContent<-try(dget(fn_file, keep.source = FALSE))
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
  fileContent<-try(dget(fn_file, keep.source = FALSE))
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
  fileContent<-try(dget(fn_file, keep.source = FALSE))
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
  fileContent<-try(dget(fn_file, keep.source = FALSE))
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

retrieve.xml<-function(fn_file=NULL){
  fileContent<-try(XML::xmlParse(fn_file))
  if (exists('fileContent')){
    if (!is.null(fileContent)){
      if (inherits(fileContent,what = 'try-error')){
        stop(mError('Cannot open file archive'))
      }
    }
  }
  return(fileContent)
}
