#' Study Table content
#'
#'
#' Retrive or update content of the study table. Some content such as identifiers
#' are read only to prevent corruption of study cross references.
#'
#' @return an object of class IMC_StudyTable or a character vector (named if different from uids)
#' @seealso updateMetadata() for extend updated values to other objects in the study
#' @examples
#' \dontrun{
#' default_study<-studyTable(x = MyStudy)
#' samples(x = myStudy)<-('MySample1','MySample2','MySample3')
#' }
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("studyTable", function(x,...)
  standardGeneric("studyTable"))

#' @details studyTable: retrive the entire study table, there is no assignment method
#' @docType methods
#' @aliases StudyTable,environment-method
#' @rdname StudyTable-methods
setMethod('studyTable',signature = ('environment'),
          function(x,...){
            return(x$studyTable)})

#####st_uids####

#' @details st_uids: retrive uid column
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_uids", function(x,...)
  standardGeneric("st_uids"))


#' @docType methods
#' @aliases st_uids,environment
#' @rdname StudyTable-methods
setMethod('st_uids',signature = ('environment'),
          function(x,...){
            uids<-x$studyTable$uid
            return(uids)})

#' @docType methods
#' @aliases st_uids,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_uids',signature = ('IMC_StudyTable'),
          function(x,...){
            uids<-x$uid
            return(uids)})


#####st_uids<-####

#' @details st_uids<-: prevent uid modifications
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_uids<-", function(x,...)
  standardGeneric("st_uids<-"))


#' @docType methods
#' @aliases st_uids<-,environment
#' @rdname StudyTable-methods
setMethod('st_uids<-',signature = ('environment'),
          function(x,...){
            stop(RUNIMC:::mError('not allowed to change uids'),call. = F)})

#' @docType methods
#' @aliases st_uids<-,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_uids<-',signature = ('IMC_StudyTable'),
          function(x,...){
            stop(RUNIMC:::mError('not allowed to change uids'),call. = F)})

#####st_folders####

#' @details st_folders: retrive IMC_folder column
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_folders", function(x,...)
  standardGeneric("st_folders"))


#' @docType methods
#' @aliases st_folders,environment
#' @rdname StudyTable-methods
setMethod('st_folders',signature = ('environment'),
          function(x,...){
            uids<-x$studyTable$uid
            out<-x$studyTable$IMC_folder
            names(out)<-uids
            return(out)})

#' @docType methods
#' @aliases st_folders,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_folders',signature = ('IMC_StudyTable'),
          function(x,...){
            uids<-x$uid
            out<-x$IMC_folder
            names(out)<-uids
            return(out)})


#####st_folders<-####

#' @details st_folders<-: prevent IMCfolder modifications
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_folders<-", function(x,...)
  standardGeneric("st_folders<-"))


#' @docType methods
#' @aliases st_folders<-,environment
#' @rdname StudyTable-methods
setMethod('st_folders<-',signature = ('environment'),
          function(x,...){
            stop(RUNIMC:::mError('not allowed to change IMC_folder'),call. = F)})

#' @docType methods
#' @aliases st_folders<-,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_folders<-',signature = ('IMC_StudyTable'),
          function(x,...){
            stop(RUNIMC:::mError('not allowed to change IMC_folder'),call. = F)})


#####st_files####

#' @details st_files: retrive IMC_text_file column
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_files", function(x,...)
  standardGeneric("st_files"))


#' @docType methods
#' @aliases st_files,environment
#' @rdname StudyTable-methods
setMethod('st_files',signature = ('environment'),
          function(x,...){
            uids<-x$studyTable$uid
            out<-x$studyTable$IMC_text_file
            names(out)<-uids
            return(out)})

#' @docType methods
#' @aliases st_files,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_files',signature = ('IMC_StudyTable'),
          function(x,...){
            uids<-x$uid
            out<-x$IMC_text_file
            names(out)<-uids
            return(out)})


#####st_files<-####

#' @details st_files<-: prevent folder modifications
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_files<-", function(x,...)
  standardGeneric("st_files<-"))


#' @docType methods
#' @aliases st_files<-,environment
#' @rdname StudyTable-methods
setMethod('st_files<-',signature = ('environment'),
          function(x,...){
            stop(RUNIMC:::mError('not allowed to change IMC_text_file'),call. = F)})

#' @docType methods
#' @aliases st_files<-,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_files<-',signature = ('IMC_StudyTable'),
          function(x,...){
            stop(RUNIMC:::mError('not allowed to change IMC_text_file'),call. = F)})

#####samples####

#' @details st_samples: retrive IMC_text_file column
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_samples", function(x,...)
  standardGeneric("st_samples"))


#' @docType methods
#' @aliases samples,environment
#' @rdname StudyTable-methods
setMethod('st_samples',signature = ('environment'),
          function(x,...){
            uids<-x$studyTable$uid
            out<-x$studyTable$sample
            names(out)<-uids
            return(out)})

#' @docType methods
#' @aliases IMCfile,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_samples',signature = ('IMC_StudyTable'),
          function(x,...){
            uids<-x$uid
            out<-x$sample
            names(out)<-uids
            return(out)})


#####samples<-####

#' @details samples<-: modify sample names
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_samples<-", function(x,value,...)
  standardGeneric("st_samples<-"))


#' @docType methods
#' @aliases st_samples<-,environment
#' @rdname StudyTable-methods
setMethod('st_samples<-',signature = ('environment'),
          function(x,value,...){
            if (!is.character(value)){
              value<-try(as.character(value))
              if (inherits(value,'try-error')) stop (RUNIMC:::mError('cannot coerce new vector to character'),call. = F)
            }
            if (length(value)!=nrow(x$studyTable)) stop(RUNIMC:::mError('length of names does not match number of files'))
            x$studyTable$sample<-value
            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$studyTable,'mdtnTimeStmp')<-newTimeStmp
            return(x)
            })

#' @docType methods
#' @aliases st_samples<-,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_samples<-',signature = ('IMC_StudyTable'),
          function(x,value,...){
            if (!is.character(value)){
              value<-try(as.character(value))
              if (inherits(value,'try-error')) stop (RUNIMC:::mError('cannot coerce new vector to character'),call. = F)
            }
            if (length(value)!=nrow(x)) stop(RUNIMC:::mError('length of names does not match number of files'))
            x$sample<-value
            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            return(x)
            })

#####st_replicates####

#' @details st_replicates: retrive IMC_text_file column
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_replicates", function(x,...)
  standardGeneric("st_replicates"))


#' @docType methods
#' @aliases st_replicates,environment
#' @rdname StudyTable-methods
setMethod('st_replicates',signature = ('environment'),
          function(x,...){
            uids<-x$studyTable$uid
            out<-x$studyTable$replicate
            names(out)<-uids
            return(out)})

#' @docType methods
#' @aliases st_replicates,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_replicates',signature = ('IMC_StudyTable'),
          function(x,...){
            uids<-x$uid
            out<-x$replicate
            names(out)<-uids
            return(out)})


#####st_replicates<-####

#' @details st_replicates<-: modify replicate names
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_replicates<-", function(x,value,...)
  standardGeneric("st_replicates<-"))


#' @docType methods
#' @aliases st_replicates<-,environment
#' @rdname StudyTable-methods
setMethod('st_replicates<-',signature = ('environment'),
          function(x,value,...){
            if (!is.character(value)){
              value<-try(as.character(value))
              if (inherits(value,'try-error')) stop (RUNIMC:::mError('cannot coerce new vector to character'),call. = F)
            }
            if (length(value)!=nrow(x$studyTable)) stop(RUNIMC:::mError('length of names does not match number of files'))
            x$studyTable$replicate<-value
            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$studyTable,'mdtnTimeStmp')<-newTimeStmp
            return(x)
          })

#' @docType methods
#' @aliases st_replicates<-,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_replicates<-',signature = ('IMC_StudyTable'),
          function(x,value,...){
            if (!is.character(value)){
              value<-try(as.character(value))
              if (inherits(value,'try-error')) stop (RUNIMC:::mError('cannot coerce new vector to character'),call. = F)
            }
            if (length(value)!=nrow(x)) stop(RUNIMC:::mError('length of names does not match number of files'))
            x$replicate<-value
            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            return(x)
          })


#####st_rois####

#' @details IMCfile: retrive IMC_text_file column
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_rois", function(x,...)
  standardGeneric("st_rois"))


#' @docType methods
#' @aliases st_rois,environment
#' @rdname StudyTable-methods
setMethod('st_rois',signature = ('environment'),
          function(x,...){
            uids<-x$studyTable$uid
            out<-x$studyTable$ROI
            names(out)<-uids
            return(out)})

#' @docType methods
#' @aliases st_rois,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_rois',signature = ('IMC_StudyTable'),
          function(x,...){
            uids<-x$uid
            out<-x$ROI
            names(out)<-uids
            return(out)})


#####st_rois<-####

#' @details st_rois<-: modify region of interest (ROI) names
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_rois<-", function(x,value,...)
  standardGeneric("st_rois<-"))


#' @docType methods
#' @aliases st_rois<-,environment
#' @rdname StudyTable-methods
setMethod('st_rois<-',signature = ('environment'),
          function(x,value,...){
            if (!is.character(value)){
              value<-try(as.character(value))
              if (inherits(value,'try-error')) stop (RUNIMC:::mError('cannot coerce new vector to character'),call. = F)
            }
            if (length(value)!=nrow(x$studyTable)) stop(RUNIMC:::mError('length of names does not match number of files'))
            x$studyTable$ROI<-value
            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$studyTable,'mdtnTimeStmp')<-newTimeStmp
            return(x)
          })

#' @docType methods
#' @aliases st_rois<-,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_rois<-',signature = ('IMC_StudyTable'),
          function(x,value,...){
            if (!is.character(value)){
              value<-try(as.character(value))
              if (inherits(value,'try-error')) stop (RUNIMC:::mError('cannot coerce new vector to character'),call. = F)
            }
            if (length(value)!=nrow(x)) stop(RUNIMC:::mError('length of names does not match number of files'))
            x$ROI<-value
            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            return(x)
          })

#####st_bioGroups####

#' @details IMCfile: retrive IMC_text_file column
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_bioGroups", function(x,...)
  standardGeneric("st_bioGroups"))


#' @docType methods
#' @aliases st_bioGroups,environment
#' @rdname StudyTable-methods
setMethod('st_bioGroups',signature = ('environment'),
          function(x,...){
            uids<-x$studyTable$uid
            out<-x$studyTable$bioGroup
            names(out)<-uids
            return(out)})

#' @docType methods
#' @aliases st_bioGroups,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_bioGroups',signature = ('IMC_StudyTable'),
          function(x,...){
            uids<-x$uid
            out<-x$bioGroup
            names(out)<-uids
            return(out)})


#####bioGroups<-####

#' @details st_bioGroups<-: modify bio group (eg. healthy control, treated, tumor... ) names
#' @export
#' @docType methods
#' @rdname StudyTable-methods
setGeneric("st_bioGroups<-", function(x,value,...)
  standardGeneric("st_bioGroups<-"))


#' @docType methods
#' @aliases st_bioGroups<-,environment
#' @rdname StudyTable-methods
setMethod('st_bioGroups<-',signature = ('environment'),
          function(x,value,...){
            if (!is.character(value)){
              value<-try(as.character(value))
              if (inherits(value,'try-error')) stop (RUNIMC:::mError('cannot coerce new vector to character'),call. = F)
            }
            if (length(value)!=nrow(x$studyTable)) stop(RUNIMC:::mError('length of names does not match number of files'))
            x$studyTable$bioGroup<-value
            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$studyTable,'mdtnTimeStmp')<-newTimeStmp
            return(x)
          })

#' @docType methods
#' @aliases st_bioGroups<-,IMC_StudyTable
#' @rdname StudyTable-methods
setMethod('st_bioGroups<-',signature = ('IMC_StudyTable'),
          function(x,value,...){
            if (!is.character(value)){
              value<-try(as.character(value))
              if (inherits(value,'try-error')) stop (RUNIMC:::mError('cannot coerce new vector to character'),call. = F)
            }
            if (length(value)!=nrow(x)) stop(RUNIMC:::mError('length of names does not match number of files'))
            x$bioGroup<-value
            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            return(x)
          })


