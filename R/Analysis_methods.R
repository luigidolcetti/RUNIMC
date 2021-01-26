#' Create a new Analysis
#'
#' An IMC analysis is composed of few objects contained in the environment
#'   **currentAnalysis** and a corresponding sub-folder of the study folder.
#'   [newAnalysis()] create an empty analysis structure both in the specified
#'   study environment and on disk, essentially overwriting the analysis object
#'   but keeping the file structure, so a previous analysis can always be loaded
#'   as current via the [retrieve()] method.
#'
#' @param x environment, a study.
#' @param analysisName character, name of the new analysis. This name needs to be
#'   a valid R name and a valid name for a directory. The same name cannot be used
#'   twice within the same study.
#' @return a new analysis environment in MyStudy$currentAnalysis.
#' @examples
#' \dontrun{
#' newAnalysis(MyStudy, 'MyFirstAnalysis')
#' }
#' @export
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
#' Dismiss an analysis
#'
#' Dismissing an analysis means removing the current or a previous
#'   analysis from the specified study. Associated files are not
#'   permanently removed but copied to a **dismissed** subfolder in
#'   the analysis folder of the specified study.
#'
#' @param x environment, a study.
#' @param analysis character, name of the analysis to dismiss.
#'   If NULL the **currentAnalysis** will be dismissed.
#' @return NULL.
#' @examples
#' \dontrun{
#' dismissAnalysis(MyStudy, 'MyUnwantedAnalysis')
#' }
#' @export
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
#' List all the analysis
#'
#' List the analysis that have been created and not dismissed
#'   for the specified study.
#'
#' @param x environment, a study.
#' @return character vector.
#' @examples
#' \dontrun{
#' listAnalysis(MyStudy)
#' }
#' @export
setGeneric("an_listAnalysis", function(x,...)
  standardGeneric("an_listAnalysis"))

setMethod('an_listAnalysis',signature = ('environment'),
          function(x,...){
            x$analysis

          })

#** an_showCurrentAnalysis ---------------------------------------------------
#' Show the current analysis
#'
#' Current analysis is an environment containing multiple objects.
#' [an_showCurrentAnalysis()] lists the objects contained in the currentAnalysis
#' environment and assess whether each object is **initialized** or **empty**.
#'
#' @param x environment, a study.
#' @return print to the console a list of the current analysis objects ant their states.
#' @examples
#' \dontrun{
#' an_showCurrentAnalysis(MyStudy)
#' }
#' @export
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
