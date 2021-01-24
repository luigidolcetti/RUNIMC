#' Analysis
#'
#' Each study can have only one current analysis (myStudy$currentAnalysis) allocated in the environment
#' container currentAnalysis.
#' When creating a new analysis this take place of the currentent analysis (whenever it exists),
#' however an old analysis that has been archived can be retrieved as well.
#'
#'
#' @param x environment, a study
#' @param analysisName character, a name for a new analysis
#' @param analysis character, name of the analysis to operate on
#' @param ... not implemented
#' @return an object of class environment within the specified study
#' @seealso
#' @examples
#' \dontrun{
#' newAnalysis(x = MyStudy)
#' an_listAnalysis (x = MyStudy)
#' an_currentAnalysis (x = MyStudy)
#' an_dismissAnalysis (x = MyStudy) # dismiss current analysis
#' an_dismissAnalysis (x = Mystudy, analysis = 'example_analysis')
#' }
#' @export
#' @docType methods
#' @rdname Analysis-methods
setGeneric("newAnalysis", function(x,analysisName=NULL,...)
  standardGeneric("newAnalysis"))

setMethod('newAnalysis',signature = ('environment'),
          function(x,analysisName=NULL,...){


            if (is.null(analysisName)) {stop(mError('\nPlease, provide a name for this analysis'))}

            analysisNameRevised<-make.names(analysisName)

            if (analysisName!=analysisNameRevised) {message(mWarning(paste0('\nAnalysis name: "',fn_analysisName,'" has been chnaged to "',fn_analysisNameRevised,'"')))}

            if (any(analysisName %in% x$analysis)) {stop(mError(paste0('\n"',analysisName,'" already exist')))}

            # x$currentAnalysis<-new('IMC_Analysis',parent=x)
            x$currentAnalysis<-new.env(parent=x)
            x$currentAnalysis<-initObjectAttr(x$currentAnalysis)
            x$currentAnalysis$name<-analysisName
            x$currentAnalysis$folder<-checkDir(paste(x$rootFolder,x$name,'analysis',sep='/'),x$currentAnalysis$name)

            checkDir(x$currentAnalysis$folder,'rasters')
            checkDir(x$currentAnalysis$folder,'rasterStacks')
            checkDir(x$currentAnalysis$folder,'training')
            checkDir(x$currentAnalysis$folder,'training/polygons')
            checkDir(x$currentAnalysis$folder,'test')
            checkDir(x$currentAnalysis$folder,'test/polygons')
            checkDir(x$currentAnalysis$folder,'test/classification')

            x$currentAnalysis$filters<-NULL
            x$currentAnalysis$derivedRasters<-NULL
            x$currentAnalysis$extractionDirectives<-NULL
            x$currentAnalysis$trainingFeatures <-NULL
            x$currentAnalysis$classificationDirectives <-NULL
            x$currentAnalysis$classifier <-NULL
            x$currentAnalysis$classification <-NULL
            x$currentAnalysis$interpretationMatrix<-NULL
            x$currentAnalysis$segmentationDirectives <-NULL
            x$currentAnalysis$segmentation <-NULL
            x$currentAnalysis$exprs <-NULL


            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            if (is.null(x$analysis)){
              x$analysis<-x$currentAnalysis$name} else {
                x$analysis<-append(x$analysis,x$currentAnalysis$name)
              }
          })

#** dismissAnalisys ---------------------------------------------------
#' @details an_dismissAnalysis: delete an analysis and the associated files
#' @export
#' @docType methods
#' @rdname Analysis-methods
setGeneric("an_dismissAnalysis", function(x,analysis=NULL,...)
  standardGeneric("an_dismissAnalysis"))

setMethod('an_dismissAnalysis',signature = ('environment'),
          function(x,analysis=NULL,...){

            dsmsFolder<-checkDir(file.path(x$rootFolder,x$name,'analysis'),'dismissed')
            if (is.null(x$analysis)) stop(mError('There are no analysis to dismiss'))
            if (!is.null(analysis) & !is.null(x$currentAnalysis)){
              if (analysis==x$currentAnalysis$name) analysis<-NULL
            }
            if (is.null(analysis)) {
              message(mWarning('Current analysis will be dismissed, continue?'))
              answr<-readline()
              if (!grepl("y",answr,ignore.case = T)) stop(mWarning('Dismissal aborted'),call. = F)
              analysis<-x$currentAnalysis$name
              x$analysis<-x$analysis[x$analysis!=analysis]
              file.copy(from = x$currentAnalysis$folder,to = dsmsFolder,overwrite = T,recursive = T)
              unlink(x$currentAnalysis$folder,recursive = T,force = T)
              x$currentAnalysis<-NULL
              return(message(mMessage(paste0(analysis, ' dismissed'))))
            }
            lstAnl<-x$analysis
            if (!any(analysis %in% lstAnl)) stop(mError("Specified analysis doesn't exist in this study"),call. = F)
            file.copy(from = file.path(x$rootFolder,x$name,'analysis',analysis),to = dsmsFolder,overwrite = T,recursive = T)
            unlink(file.path(x$rootFolder,x$name,'analysis',analysis),recursive = T,force = T)
            x$analysis<-x$analysis[x$analysis!=analysis]
            return(message(mMessage(paste0(analysis, ' dismissed'))))
          })

setMethod('an_dismissAnalysis',signature = c('missing'),
          function(x,analysis=NULL,...){
            stop(mError('Specify study'))
          })

#** listAnalisys ---------------------------------------------------
#' @details an_listAnalysis: list the analysis created in the specified study
#' @export
#' @docType methods
#' @rdname Analysis-methods
setGeneric("an_listAnalysis", function(x,...)
  standardGeneric("an_listAnalysis"))

setMethod('an_listAnalysis',signature = ('environment'),
          function(x,...){
            x$analysis

          })

#** showCurrentAnalysis ---------------------------------------------------
#' @details an_showCurrentAnalysis: show a summary of the currentAnalysis
#' @export
#' @docType methods
#' @rdname Analysis-methods
setGeneric("an_showCurrentAnalysis", function(x,...)
  standardGeneric("an_showCurrentAnalysis"))

setMethod('an_showCurrentAnalysis',signature = ('environment'),
          function(x,...){
            if (is.null(x$currentAnalysis)) {
              message(mWarning('Current Analysis is empty'))
            }else {
              stuffInThere<-ls(name = x$currentAnalysis)
              cat(paste0('Current analysis: ',x$currentAnalysis$name,'\n'))
              for (i in stuffInThere){
                if (is.null(x$currentAnalysis[[i]])){
                  cat(paste0(i,': ',mWarning('empty'),'\n'))
                } else (cat(paste0(i, ': ',mMessage('initialized'),'\n')))
              }
            }
          })
