#methods ---------------------------------------------------
#* initialize ---------------------------------------------------
#** initialize.channeltable ---------------------------------------------------
setMethod('initialize','IMC_ChannelTable',
          function(.Object,val = NULL, ...) {
            Object <- callNextMethod(.Object, ...)
            Object@.Data <- val
            Object@names <- names(val)
            Object@row.names <- row.names(val)
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.studytable ---------------------------------------------------
setMethod('initialize','IMC_StudyTable',
          function(.Object,val=NULL, ...) {
            Object <- callNextMethod(.Object, ...)
            Object@.Data <- val
            Object@names <- names(val)
            Object@row.names <- row.names(val)
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.RasterStack ---------------------------------------------------
setMethod('initialize','IMC_RasterStack',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.RsCollection ---------------------------------------------------
setMethod('initialize','IMC_RsCollection',
          function(.Object,val, ...) {
            Object <- callNextMethod(.Object, ...)

            Object@.Data <- val
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.TrainingFeatures ---------------------------------------------------
setMethod('initialize','IMC_TrainingFeatures',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.InterpretationMatrix ---------------------------------------------------
setMethod('initialize','IMC_InterpretationMatrix',
          function(.Object,val, ...) {
            Object <- callNextMethod(.Object, ...)
            Object@.Data <- val
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.Classifier ---------------------------------------------------
setMethod('initialize','IMC_Classifier',
          function(.Object,val, ...) {
            Object <- callNextMethod(.Object, ...)
            if(!is.null(val)){
              Object@.Data <- val
            }
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.Segmentation ---------------------------------------------------
setMethod('initialize','IMC_Segmentation',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.FilterFrame ---------------------------------------------------
setMethod('initialize','IMC_FilterFrame',
          function(.Object,val=NULL, ...) {
            Object <- callNextMethod(.Object, ...)
            if(!is.null(val)){
              Object@.Data <- val
              Object@names <- names(val)
              Object@row.names <- row.names(val)
            }
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.extractionDirectives---------------------------------------------------
setMethod('initialize','IMC_ExtractionDirectives',
          function(.Object,val=NULL, ...) {
            Object <- callNextMethod(.Object, ...)

            if(!is.null(val)){
              Object@.Data <- val
              Object@names <- names(val)
              Object@row.names <- row.names(val)
            }
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.trainingfeatures---------------------------------------------------
setMethod('initialize','IMC_TrainingFeatures',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.classificationDirectives ---------------------------------------------------
setMethod('initialize','IMC_ClassificationDirectives',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.TrainingFeatures ---------------------------------------------------
setMethod('initialize','IMC_ClassificationDirectivesList',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.classification ---------------------------------------------------
setMethod('initialize','IMC_Classification',
          function(.Object,val, ...) {
            Object <- callNextMethod(.Object, ...)
            Object@.Data <- val
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.classificationDirectives ---------------------------------------------------
setMethod('initialize','IMC_SegmentationDirectives',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.segmentationlist ---------------------------------------------------
setMethod('initialize','IMC_SegmentationList',
          function(.Object,val, ...) {
            Object <- callNextMethod(.Object, ...)
            Object@.Data <- val
            Object<-initObjectAttr(Object)
            return(Object)})

#** initialize.expressionmatrix ---------------------------------------------------
# setMethod('initialize','IMC_ExpressionMatrix',
#           function(.Object, ...) {
#             Object <- callNextMethod(.Object, ...)
#             Object<-initObjectAttr(Object)
#             return(Object)})


#** initialize.Analysis ---------------------------------------------------
# setMethod("initialize", "IMC_Analysis",
#           function(.Object,name,parent = parent.frame(), ...) {
#             Object <- callNextMethod(.Object, ...)
#             parent.env(Object@.xData)<-parent
#             Object@.xData<-initObjectAttr(Object@.xData)
#             return(Object)
#           })

#** initialize.AnalysisList ---------------------------------------------------
setMethod('initialize','IMC_AnalysisList',
          function(.Object, ...) {
            Object <- callNextMethod(.Object, ...)
            Object<-initObjectAttr(Object)
            return(Object)}
)

#** initialize.Study ---------------------------------------------------
# setMethod("initialize", "IMC_Study",
#           function(.Object,name,parent = parent.frame(), ...) {
#             Object <- callNextMethod(.Object, ...)
#             parent.env(Object@.xData)<-parent
#             Object@.xData<-initObjectAttr(Object@.xData)
#             return(Object)})

#* Study.specific_methods ---------------------------------------------------
#** newAnalysis (moved)---------------------------------------------------
# if (!isGeneric("newAnalysis")) {
#   setGeneric("newAnalysis", function(x,analysisName=NULL,...)
#     standardGeneric("newAnalysis"))
# }
#
# setMethod('newAnalysis',signature = ('environment'),
#           function(x,analysisName=NULL,...){
#
#
#             if (is.null(analysisName)) {stop(mError('\nPlease, provide a name for this analysis'))}
#
#             analysisNameRevised<-make.names(analysisName)
#
#             if (analysisName!=analysisNameRevised) {message(mWarning(paste0('\nAnalysis name: "',fn_analysisName,'" has been chnaged to "',fn_analysisNameRevised,'"')))}
#
#             if (any(analysisName %in% x$analysis)) {stop(mError(paste0('\n"',analysisName,'" already exist')))}
#
#             # x$currentAnalysis<-new('IMC_Analysis',parent=x)
#             x$currentAnalysis<-new.env(parent=x)
#             x$currentAnalysis<-initObjectAttr(x$currentAnalysis)
#             x$currentAnalysis$name<-analysisName
#             x$currentAnalysis$folder<-checkDir(paste(x$rootFolder,x$name,'analysis',sep='/'),x$currentAnalysis$name)
#
#             checkDir(x$currentAnalysis$folder,'rasters')
#             checkDir(x$currentAnalysis$folder,'rasterStacks')
#             checkDir(x$currentAnalysis$folder,'training')
#             checkDir(x$currentAnalysis$folder,'training/polygons')
#             checkDir(x$currentAnalysis$folder,'test')
#             checkDir(x$currentAnalysis$folder,'test/polygons')
#             checkDir(x$currentAnalysis$folder,'test/classification')
#
#             x$currentAnalysis$filters<-NULL
#             x$currentAnalysis$derivedRasters<-NULL
#             x$currentAnalysis$extractionDirectives<-NULL
#             x$currentAnalysis$trainingFeatures <-NULL
#             x$currentAnalysis$classificationDirectives <-NULL
#             x$currentAnalysis$classifier <-NULL
#             x$currentAnalysis$classification <-NULL
#             x$currentAnalysis$interpretationMatrix<-NULL
#             x$currentAnalysis$segmentationDirectives <-NULL
#             x$currentAnalysis$segmentation <-NULL
#             x$currentAnalysis$exprs <-NULL
#
#
#             newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#             attr(x,'mdtnTimeStmp')<-newTimeStmp
#             if (is.null(x$analysis)){
#               x$analysis<-x$currentAnalysis$name} else {
#                 x$analysis<-append(x$analysis,x$currentAnalysis$name)
#               }
#           })
#
# #** dismissAnalisys (moved)---------------------------------------------------
# if (!isGeneric("dismissAnalysis")) {
#   setGeneric("dismissAnalysis", function(x,analysis=NULL,...)
#     standardGeneric("dismissAnalysis"))
# }
#
# setMethod('dismissAnalysis',signature = ('environment'),
#           function(x,analysis=NULL,...){
#
#             dsmsFolder<-checkDir(file.path(x$rootFolder,x$name,'analysis'),'dismissed')
#             if (is.null(x$analysis)) stop(mError('There are no analysis to dismiss'))
#             if (analysis==x$currentAnalysis$name) analysis<-NULL
#             if (is.null(analysis)) {
#               message(mWarning('No analysis name specified, current analysis will be dismissed, continue?'))
#               answr<-readline()
#               if (!grepl("y",answr,ignore.case = T)) stop(mWarning('Dismissal aborted'))
#               analysis<-x$currentAnalysis$name
#               x$analysis<-x$analysis[x$analysis!=analysis]
#               file.copy(from = x$currentAnalysis$folder,to = dsmsFolder,overwrite = T,recursive = T)
#               unlink(x$currentAnalysis$folder,recursive = T,force = T)
#               x$currentAnalysis<-NULL
#               message(mMessage(paste0(analysis, ' dismissed')))
#               return(0)
#             }
#             lstAnl<-listAnalysis(x)
#             if (!any(analysis %in% lstAnl)) stop(mError("Specified analysis doesn't exist in this study"))
#             file.copy(from = file.path(x$rootFolder,x$name,'analysis',analysis),to = dsmsFolder,overwrite = T,recursive = T)
#             unlink(file.path(x$rootFolder,x$name,'analysis',analysis),recursive = T,force = T)
#             x$analysis<-x$analysis[x$analysis!=analysis]
#             return(mMessage(paste0(analysis, ' dismissed')))
#           })
#
# setMethod('dismissAnalysis',signature = c('missing'),
#           function(x,analysis=NULL,...){
#             stop(mError('Specify study'))
#           })
#
# #** listAnalisys (moved)---------------------------------------------------
# if (!isGeneric("listAnalysis")) {
#   setGeneric("listAnalysis", function(x,...)
#     standardGeneric("listAnalysis"))
# }
#
# setMethod('listAnalysis',signature = ('environment'),
#           function(x,...){
#             x$analysis
#           })
#
# #** showCurrentAnalysis (moved)---------------------------------------------------
#
# if (!isGeneric("showCurrentAnalysis")) {
#   setGeneric("showCurrentAnalysis", function(x,...)
#     standardGeneric("showCurrentAnalysis"))
# }
#
# setMethod('showCurrentAnalysis',signature = ('environment'),
#           function(x,...){
#             x$currentAnalysis
#           })

# #** channels (moved)---------------------------------------------------
# if (!isGeneric("channels")) {
#   setGeneric("channels", function(x, ...)
#     standardGeneric("channels"))
# }
#
#
# setMethod('channels',signature = ('ANY'),
#           function(x,...){
#             x$channels
#           })
#
# #** rasterFromMarker (moved)---------------------------------------------------
# if (!isGeneric("rasterFromMarker")) {
#   setGeneric("rasterFromMarker", function(x, marker,...)
#     standardGeneric("rasterFromMarker"))
# }
#
#
# setMethod('rasterFromMarker',signature = ('ANY'),
#           function(x, marker,...){
#             chnls<-x@channels$RcolumnNames[grep(marker,
#                                                 x@channels$marker,
#                                                 ignore.case = T)]
#             names(chnls)<-x@channels$marker[grep(marker,
#                                                  x@channels$marker,
#                                                  ignore.case = T)]
#             return(chnls)
#           })
#
# #** channelsAll (moved)---------------------------------------------------
# if (!isGeneric("channelsAll")) {
#   setGeneric("channelsAll", function(x, ...)
#     standardGeneric("channelsAll"))
# }
#
#
# setMethod('channelsAll',signature = ('ANY'),
#           function(x,...){
#             x$channels$RcolumnNames[x$channels$loaded]
#           })

# #** addFilter (moved)---------------------------------------------------
# if (!isGeneric("addFilter")) {
#   setGeneric("addFilter", function(x,filter=NULL,parameters=NULL,channels=NULL,append=T,...)
#     standardGeneric("addFilter"))
# }
#
# setMethod('addFilter',signature = ('environment'),
#           function(x,filter=NULL,parameters=NULL,channels=NULL,append=T,...){
#
#             if (is.null(filter)) stop(mError('provide Filter'))
#             if (is.null(parameters)) stop(mError(paste0('provide parameters for ',filter,' filter')))
#             if (is.null(channels)) stop(mError(paste0('provide channels to elaborate with ',filter,' filter')))
#             if (append) x$currentAnalysis$filters<-x$currentAnalysis$filters else x$currentAnalysis$filters<-NULL
#             if (is.null(x$currentAnalysis$filters)) {
#               newFilter<-new('IMC_FilterFrame')
#               x$currentAnalysis$filters<-newFilter
#             }
#
#             newFilter<-data.frame(filter=filter,
#                                   parameters=I(list(parameters)),
#                                   channels=I(list(channels)))
#
#
#             oldFilters<-x$currentAnalysis$filters
#             newFilter<-rbind.data.frame(oldFilters,newFilter)
#             newFilter<-new('IMC_FilterFrame',newFilter)
#
#             newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#             attr(x,'mdtnTimeStmp')<-newTimeStmp
#             attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
#             x$currentAnalysis$filters<-newFilter
#
#           })
#
# #** deployFilters (moved)---------------------------------------------------
# if (!isGeneric("deployFilters")) {
#   setGeneric("deployFilters", function(x,saveToDisk=T, ...)
#     standardGeneric("deployFilters"))
# }
#
# setMethod('deployFilters',signature = ('environment'),
#           function(x,saveToDisk=T...){
#
#             if (saveToDisk){
#               oldRstk<-list.files(file.path(x$currentAnalysis$folder,'rasterStacks'),full.names = T,recursive = F)
#               unlink(oldRstk,recursive = T)
#               oldRst<-list.dirs(file.path(x$currentAnalysis$folder,'rasters'),full.names = T,recursive = F)
#               unlink(oldRst,recursive = T)
#               message(mMessage('Clean up raster and rasterStack folders'))
#             }
#
#             ff<-x$currentAnalysis$filters
#             derivedRasters<-apply(ff,1,function(fltDf){
#               imageRFilter(fn_rasterStack = x$raster,
#                            fn_filter = fltDf$filter,
#                            fn_filterParameterList = fltDf$parameters,
#                            fn_markerList = fltDf$channels,
#                            fn_saveToDisk = saveToDisk,
#                            fn_pathToFile = x$currentAnalysis$folder)
#             })
#
#
#             rstNms<-Reduce('=',lapply(derivedRasters,names))
#             derivedRasters<-unlist(derivedRasters,recursive=F)
#             derivedRasters<-sapply(rstNms,function(x){
#               rasterStackList<-derivedRasters[rstNms==x]
#
#
#               uid<-Reduce('=',lapply(rasterStackList,slot,'uid'))
#               IMC_text_file<-Reduce('=',lapply(rasterStackList,slot,'IMC_text_file'))
#               study<-Reduce('=',lapply(rasterStackList,slot,'study'))
#               sample<-Reduce('=',lapply(rasterStackList,slot,'sample'))
#               replicate<-Reduce('=',lapply(rasterStackList,slot,'replicate'))
#               ROI<-Reduce('=',lapply(rasterStackList,slot,'ROI'))
#               bioGroup<-Reduce('=',lapply(rasterStackList,slot,'bioGroup'))
#               channels<-Reduce('=',lapply(rasterStackList,slot,'channels'))
#               IMC_stack(rasterStackList,
#                         uid,
#                         IMC_text_file,
#                         study,
#                         sample,
#                         replicate,
#                         ROI,
#                         bioGroup,
#                         channels)},USE.NAMES = T)
#
#             if (saveToDisk){
#               derivedRasters<-sapply(names(derivedRasters),function(nms){
#                 IMCstackSave(derivedRasters[[nms]],
#                              file.path(x$currentAnalysis$folder,
#                                        'rasterStacks',
#                                        paste0(derivedRasters[[nms]]@IMC_text_file,'.stk')))
#               },USE.NAMES = T,simplify = F)
#             }
#
#             derivedRasters<-new('IMC_RsCollection',derivedRasters)
#
#             newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#             attr(x,'mdtnTimeStmp')<-newTimeStmp
#             attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
#
#             x$currentAnalysis$derivedRasters<-derivedRasters
#
#             if (saveToDisk){
#               attr(x$currentAnalysis$derivedRasters,'mdtnTimeStmp')<-newTimeStmp
#               attr(x$currentAnalysis$derivedRasters,'artnTimeStmp')<-newTimeStmp
#               attr(x$currentAnalysis$derivedRasters,'fileArchive')<-file.path(x$currentAnalysis$folder,'rasterStacks')
#
#             }
#
#           })

#** addExtractionDirectives ---------------------------------------------------
#' Extraction Directives
#'
#' Training feature directives determine how training features (pixels) are extracted
#'   from training data. [addExtractionDirectives()] either initialize or add a new
#'   row to the table of directives.
#'
#' @param x environment, a study
#' @param coverage numeric, a pair of values between 0 and 1 that specify the range
#'   of coverage to be included in the category
#' @param prefix character, a prefix that will be added to each label in the form
#'   prefix_label. It must not contain any underscore
#' @param append logic, initialize (FALSE) or append a new definition (TRUE)
#' @param ... not implemented
#' @examples
#' \dontrun{
#' addExtractionDirectives(MyStudy,c(0.75,100),'core', append = F)
#' addExtractionDirectives(MyStudy,c(0,0.75),'border', append = T)
#' }
#' @export
setGeneric("addExtractionDirectives", function(x,
                                               coverage=NULL,
                                               prefix=NULL,
                                               append=T,
                                               ...)
  standardGeneric("addExtractionDirectives"))


setMethod('addExtractionDirectives',signature = ('environment'),
          function(x,
                   coverage=NULL,
                   prefix=NULL,
                   append=T,
                   ...){


            if (is.null(coverage)) stop(mError('provide coverage bounderies'))
            if (is.null(prefix)) stop(mError(paste0('provide prefix to use in label')))

            if (append) x$currentAnalysis$Directives<-x$currentAnalysis$extractionDirectives else x$currentAnalysis$extractionDirectives<-NULL

            if (is.null(x$currentAnalysis$extractionDirectives)) {
              newDirectives<-new('IMC_ExtractionDirectives')

              x$currentAnalysis$extractionDirectives<-newDirectives
            }

            newDirectives<-data.frame(coverage=I(list(coverage)),prefix=I(list(prefix)))

            oldDirectives<-x$currentAnalysis$extractionDirectives

            newDirectives<-rbind.data.frame(oldDirectives,newDirectives)

            newDirectives<-new('IMC_ExtractionDirectives',newDirectives)
            newDirectives<-initObjectAttr(newDirectives)
            x$currentAnalysis$extractionDirectives<-newDirectives

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
            x$currentAnalysis$extractionDirectives<-newDirectives

          })


#** extractTrainingFeatures ---------------------------------------------------
#' Extraction of training features
#'
#' Training feature (pixels) are extracted from training data, following
#'   following the directives provided via the function [addExtractionDirectives()].
#'
#' @param x environment, a study
#' @param ... not implemented
#' @examples
#' \dontrun{
#' addExtractionDirectives(MyStudy,c(0.75,100),'core', append = F)
#' extractTrainingFeatures(MyStudy)
#' }
#' @export
setGeneric("extractTrainingFeatures", function(x,...)
  standardGeneric("extractTrainingFeatures"))

setMethod('extractTrainingFeatures',signature = ('environment'),
          function(x,...){


            if (is.null(x$currentAnalysis$extractionDirectives)) stop(mError('Before extracting features extraction directives must be specified'))

            newCoverage<-c(unlist(x$currentAnalysis$extractionDirectives$coverage))
            newCoverageLabel<-c(unlist(x$currentAnalysis$extractionDirectives$prefix))

            trainingPolygonPath<-paste(x$currentAnalysis$folder,'training/Polygons',sep='/')

            trainingPolygon<-list.files(trainingPolygonPath,full.names = T)

            if (length(trainingPolygon)==0) stop(mError(paste0('Could not find any polygon file in ',trainingPolygonPath)))

            trainingFeatures<-lapply(trainingPolygon,function(tpl){
              polygonFiles<-sf::st_read(tpl)
              TEMP_UID<-as.character(Reduce('=',polygonFiles$uid))


              rst<-list(stack(x$raster[[TEMP_UID]],x$currentAnalysis$derivedRasters[[TEMP_UID]]))
              names(rst)<-TEMP_UID
              trainingMatrix<-extractFeatures(rst,
                                              polygonFiles,
                                              fn_coverage = newCoverage,
                                              fn_coverage_label = newCoverageLabel)

            })



            TEMP_value<-lapply(trainingFeatures,function(x)x$value)
            TEMP_value<-do.call(rbind.data.frame,c(TEMP_value,stringsAsFactors=F))
            TEMP_value$label<-as.factor(TEMP_value$label)
            TEMP_value$parLabel<-as.factor(TEMP_value$parLabel)
            TEMP_geometry<-lapply(trainingFeatures,function(x)x$geometry)
            TEMP_geometry<-do.call(rbind.data.frame,c(TEMP_geometry,stringsAsFactors=F))

            trainingMatrix<-list(value=TEMP_value,geometry=TEMP_geometry)
            trainingMatrix<-new('IMC_TrainingFeatures',trainingMatrix)
            trainingMatrix<-initObjectAttr(trainingMatrix)
            x$currentAnalysis$trainingFeatures<-trainingMatrix

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
          })

#** addClassificationDirectives ---------------------------------------------------
if (!isGeneric("addClassificationDirectives")) {
  setGeneric("addClassificationDirectives", function(x,method=NULL,methodParameters=NULL,...)
    standardGeneric("addClassificationDirectives"))
}

#' @export
setMethod('addClassificationDirectives',signature = ('environment'),
          function(x,method=NULL,methodParameters=NULL,...){


            if (is.null(method)) stop(mError('provide a method'),call. = F)
            if (is.null(methodParameters)) message(mWarning(paste0('no parameters for ',method, ' provided. Default will be used') ))

            switch(method,
                   randomForest = {
                     if (is.null(methodParameters)){
                       methodParameters<-methodParametersClassification[[method]]
                       methodParameters$responseVariable = 'label'
                       methodParameters$predictiveFeatures = tf_featureList(x)
                     } else {
                       namesDefault = names (methodParametersClassification[[method]])
                       namesProvided = names (methodParameters)
                       if (!all(namesProvided %in% namesDefault)) {
                         message(mWarning('some of the parameters do not match and will be ignored'))
                         methodParameters<-methodParameters[namesProvided %in% namesDefault]
                       }
                       methodParameters<-append(methodParameters,methodParametersClassification[[method]][!(namesDefault %in% namesProvided)])
                     }
                     # methodParameters<-methodParameters[unlist(lapply(methodParameters,function(x) !is.null(x)))]
                   },
                   randomOnions = {
                     if (is.null(methodParameters)){
                       methodParameters<-methodParametersClassification[[method]]
                       methodParameters$responseVariable = 'DFC'
                       methodParameters$predictiveFeatures = tf_featureList(x)
                       methodParameters$labels = tf_labelList(x)
                     } else {
                       namesDefault = names (methodParametersClassification[[method]])
                       namesProvided = names (methodParameters)
                       if (!all(namesProvided %in% namesDefault)) {
                         message(mWarning('some of the parameters do not match and will be ignored'))
                         methodParameters<-methodParameters[namesProvided %in% namesDefault]
                       }
                       methodParameters<-append(methodParameters,methodParametersClassification[[method]][!(namesDefault %in% namesProvided)])
                     }
                     # methodParameters<-methodParameters[unlist(lapply(methodParameters,function(x) !is.null(x)))]
                   },
                   stop(mError('unkwon method'),call. = F))

            newDirectives<-new('IMC_ClassificationDirectives',
                               method=method,
                               methodParameters=methodParameters)


            if (is.null(x$currentAnalysis$classificationDirectives)){
              x$currentAnalysis$classificationDirectives<-new('IMC_ClassificationDirectivesList')
            }

            x$currentAnalysis$classificationDirectives[[method]]<-newDirectives

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis$classificationDirectives,'mdtnTimeStmp')<-newTimeStmp
          })

#** makeClassificationModel ---------------------------------------------------
if (!isGeneric("makeClassificationModel")) {
  setGeneric("makeClassificationModel", function(x,method=NULL,seed=1234, ...)
    standardGeneric("makeClassificationModel"))
}

#' @export
setMethod('makeClassificationModel',signature = ('environment'),
          function(x,method=NULL,seed=1234,...){

            if (is.null(x$currentAnalysis$classificationDirectives)) stop(mError('Before making a classification model directives must be specified'))
            if (length(x$currentAnalysis$classificationDirectives)>1 &
                is.null(method)) stop(mError('specify what method to apply'))
            if (length(x$currentAnalysis$classificationDirectives)==1 &
                is.null(method)) method<-
                x$currentAnalysis$classificationDirectives[[1]]@method

            switch(method,

                   randomForest = {
                     set.seed=seed
                     rVar<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$responseVariable
                     pFtr<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$predictiveFeatures
                     cPrm<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters[!(names(x$currentAnalysis$classificationDirectives[[method]]@methodParameters) %in%c('responseVariable','predictiveFeatures'))]
                     cPrm<-cPrm[unlist(lapply(cPrm,function(x) !is.null(x)))]
                     fFormula<-eval(parse(text=paste0(rVar,'~',paste(pFtr,collapse = '+'))))
                     rFcall<-c(list(formula = fFormula,
                                    data = x$currentAnalysis$trainingFeatures$value),
                               cPrm)
                     rf_classifier <- do.call(randomForest::randomForest,rFcall)

                   },
                   randomOnions = {

                     Lvar<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$labels
                     rVar<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$responseVariable
                     pFtr<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$predictiveFeatures
                     cPrm<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters[!(names(x$currentAnalysis$classificationDirectives[[method]]@methodParameters) %in%c('responseVariable','predictiveFeatures','labels','classificationLyr','prefix'))]
                     cPrm<-cPrm[unlist(lapply(cPrm,function(x) !is.null(x)))]
                     fFormula<-eval(parse(text=paste0(rVar,'~',paste(pFtr,collapse = '+'))))
                     rf_classifier<-sapply(Lvar,function(lbl){
                       cat('Random Forest...:::',lbl,':::\n')
                       rFcall<-c(list(formula = fFormula,
                                      data = x$currentAnalysis$trainingFeatures$value[x$currentAnalysis$trainingFeatures$value$parLabel==lbl,]),
                                 cPrm)
                       rf<-do.call(randomForest::randomForest,rFcall)
                       return(rf)
                     },USE.NAMES = T,simplify = F)
                   },
                   stop(mError('unknown method')))

            if (is.null(x$currentAnalysis$classifier)){
              x$currentAnalysis$classifier<-new('IMC_Classifier',
                                                val = list(rf_classifier))
              names(x$currentAnalysis$classifier)<-method
            } else {
              x$currentAnalysis$classifier[[method]]<-rf_classifier
            }

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis$classifier,'mdtnTimeStmp')<-newTimeStmp
          })

#** classify ---------------------------------------------------
if (!isGeneric("classify")) {
  setGeneric("classify", function(x,method=NULL,saveToDisk=T,...)
    standardGeneric("classify"))
}

#' @export
setMethod('classify',signature = ('environment'),
          function(x,method=NULL,saveToDisk=T,...){

            if (is.null(x$currentAnalysis$classifier)) stop(mError('coud not find a classification model to apply'))
            # clfrClass<-paste(class(x$currentAnalysis$classifier),collapse = '_')
            if (is.null(method)) stop(mError('specify what method to use'))

            switch(method,
                   randomForest = {

                     pFtr<-x$currentAnalysis$classificationDirectives[[method]]@methodParameters$predictiveFeatures
                     rfCls<-x$currentAnalysis$classifier[[method]]
                     uids<-x$studyTable$uid

                     TEST_monkey<-sapply(uids,function(uid){


                       rst<-list(x$raster[[uid]],x$currentAnalysis$derivedRasters[[uid]])
                       rstrStk<-IMC_stack(x = rst,
                                          uid = x$raster[[uid]]@uid,
                                          IMC_text_file = x$raster[[uid]]@IMC_text_file,
                                          study = x$raster[[uid]]@study,
                                          sample = x$raster[[uid]]@sample,
                                          replicate = x$raster[[uid]]@replicate,
                                          ROI = x$raster[[uid]]@ROI,
                                          bioGroup = x$raster[[uid]]@bioGroup,
                                          channels = x$raster[[uid]]@channels)


                       # rst<-as(rst,'IMC_rasterStack')
                       # rst@uid = x$raster[[uid]]@uid
                       # rst@IMC_text_file = x$rastr[[uid]]@IMC_text_file
                       # rst@study = x$rastr[[uid]]@study
                       # rst@sample = x$rastr[[uid]]@sample
                       # rst@replicate = x$rastr[[uid]]@replicate
                       # rst@ROI = x$rastr[[uid]]@ROI
                       # rst@bioGroup = x$rastr[[uid]]@bioGroup

                       if (saveToDisk) {
                         fn_filePath<-file.path(x$currentAnalysis$folder,
                                                'test/classification')
                         checkDir(fn_filePath,'rasters')
                         checkDir(fn_filePath,'rasterStacks')
                       } else {
                         fn_filePath<-NULL
                       }

                       mf<-monkeyForest(fn_rst =rstrStk,
                                        fn_layers = pFtr,
                                        fn_newLayerName = 'label',
                                        fn_undeterminedLabel='undetermined',
                                        fn_Ptreshold=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$PvalueTreshold,
                                        fn_forest =rfCls,
                                        fn_filePath = fn_filePath)},USE.NAMES = T)

                     if (is.null(x$currentAnalysis$classification)){
                       TEST_monkey<-new('IMC_Classification',TEST_monkey)
                       x$currentAnalysis$classification<-TEST_monkey
                     } else {
                       x$currentAnalysis$classification<-append(x$currentAnalysis$classification,TEST_monkey)
                     }

                   },
                   randomOnions={

                     if (is.null(x$currentAnalysis$classification)) stop(mError('coud not find any classification'))


                     newClassification<-randomOnions(fn_rstStack=x$currentAnalysis$classification,
                                                     fn_layerLabel=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$classificationLyr,
                                                     fn_label=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$labels,
                                                     fn_prefix=x$currentAnalysis$classificationDirectives[[method]]@methodParameters$prefix,
                                                     fn_raster=x$raster,
                                                     fn_derivedRaster=x$currentAnalysis$derivedRasters,
                                                     fn_classifiers = x$currentAnalysis$classifier[[method]])
                     newClassification<-new('IMC_Classification',newClassification)


                     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

                     attr(newClassification,'crtnTimeStmp')<-attr(x$currentAnalysis$classification,'crtnTimeStmp')
                     attr(newClassification,'mdtnTimeStmp')<-newTimeStmp

                     if (!is.null(attr(x$currentAnalysis$classification,'fileArchive'))){
                       attr(newClassification,'artnTimeStmp')<-newTimeStmp
                       attr(newClassification,'fileArchive')<-attr(x$currentAnalysis$classification,'fileArchive')
                     }

                     x$currentAnalysis$classification<-newClassification

                     # param<-append(list(x=x),x$currentAnalysis$classificationDirectives[[method]]@methodParameters)
                     # do.call(RUNIMC::concentricClassification,param)
                   },
                   stop(mError('unknown method')))



            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
            if (saveToDisk) {
              attr(x$currentAnalysis$classification,'artnTimeStmp')<-newTimeStmp
              attr(x$currentAnalysis$classification,'fileArchive')<-file.path(x$currentAnalysis$folder,'test/classification/rasterStacks')
            }
          })

#** addInterpretationMatrix ---------------------------------------------------
if (!isGeneric("addInterpretationMatrix")) {
  setGeneric("addInterpretationMatrix", function(x,classifierObject=NULL,undtLabel=NULL,undtID=NULL,...)
    standardGeneric("addInterpretationMatrix"))
}

#' @export
setMethod('addInterpretationMatrix',signature = ('environment'),
          function(x,classifierObject=NULL,undtLabel=NULL,undtID=NULL,...){

            if (is.null(x$currentAnalysis$classifier)) stop(mError('coud not find a classification model to apply'),call. = F)
            if (is.null(classifierObject)) stop(mError('specify classifier'),call. = F)
            if (is.null(x$currentAnalysis$classifier[[classifierObject]])) stop(mError('cannot find specific the specified classifier object'),call. = F)

            iMat<-guideMatrix(x$currentAnalysis$classifier[[classifierObject]])

            for (i in names(iMat)){
              iMat[[i]]$seed[iMat[[i]]$level==i]<-1
              iMat[[i]]$include[iMat[[i]]$level==i]<-1
              iMat[[i]]$action[iMat[[i]]$level!=i]<-2
            }

            if (!is.null(undtLabel) & !is.null(undtID)){
              iMat<-sapply(names(iMat),function(nms){
                iMat[[nms]]<-rbind.data.frame(iMat[[nms]],data.frame(level=undtLabel,
                                                                     label=undtID,
                                                                     seed=0,
                                                                     include=1,
                                                                     action=0,
                                                                     row.names = undtLabel,
                                                                     stringsAsFactors = F))
              },USE.NAMES = T,simplify = F)
            }


            iMat<-new('IMC_InterpretationMatrix',iMat)
            x$currentAnalysis$interpretationMatrix<-iMat

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
          })

#** addSegmentationDirectives ---------------------------------------------------
if (!isGeneric("addSegmentationDirectives")) {
  setGeneric("addSegmentationDirectives", function(x,method=NULL,methodParameters=NULL,...)
    standardGeneric("addSegmentationDirectives"))
}

#' @export
setMethod('addSegmentationDirectives',signature = ('environment'),
          function(x,method=NULL,methodParameters=NULL,...){


            if (is.null(method)) stop(mError('provide a method'),call. = F)
            if (!any(method %in% names(methodParametersSegmentation))) stop(mError('unknown method'),call. = F)
            if (is.null(methodParameters)) {
              message(mWarning(paste0('no parameters provided for ',method,', default parameters will be added. You can change them manually in *@methodParameters') ))
              methodParameters<-methodParametersSegmentation[[method]]} else {
                namesDefault = names (methodParametersSegmentation[[method]])
                namesProvided = names (methodParameters)
                methodParameters<-append(methodParameters,methodParametersSegmentation[[method]][!(namesDefault %in% namesProvided)])
              }


            newDirectives<-new('IMC_SegmentationDirectives',
                               method=method,
                               methodParameters=methodParameters)

            x$currentAnalysis$segmentationDirectives<-newDirectives

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
          })



#** segment ---------------------------------------------------

if (!isGeneric("segment")) {
  setGeneric("segment", function(x,labelLayer='label',...)
    standardGeneric("segment"))
}

#' @export
setMethod('segment',signature = ('environment'),
          function(x,labelLayer='label',...){

            if (is.null(x$currentAnalysis$segmentationDirectives)) stop(mError('Before segmenting directives must be specified'))

            mthd<-x$currentAnalysis$segmentationDirectives@method
            mthdPrmtrs<-x$currentAnalysis$segmentationDirectives@methodParameters




            switch(mthd,

                   spiderMap = {

                     rstToSegment<-sapply(names(x$currentAnalysis$classification),function(nms){
                       if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) stop(RUNIMC:::mError('Check classification layer name provided'))
                       return(x$currentAnalysis$classification[[nms]][[labelLayer]])
                     },USE.NAMES = T,simplify = F)
                     iMat<-x$currentAnalysis$interpretationMatrix

                     polygonsList<-list()
                     for (rst in names(rstToSegment)){
                       for (i in names(iMat)){

                         cat(paste(rst,i,'\n',sep=":::"))
                         interpretationMatrixInstance<-iMat[[i]]
                         group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==i]
                         group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==i]
                         groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)*mthdPrmtrs$areaExpansion
                         groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)*mthdPrmtrs$roundnessExpansion

                         TEMP<-list(spiderMap(fn_srt = rstToSegment[[rst]],
                                              fn_interpret = interpretationMatrixInstance,
                                              fn_Nspikes=mthdPrmtrs$spikes,
                                              fn_radius = round(sqrt(groupAreaRange[[2]]*mthdPrmtrs$radiusExpansion/pi)),
                                              fn_densityMultiplier=mthdPrmtrs$densityMultiplier,
                                              fn_coverage = mthdPrmtrs$coverage,
                                              fn_minArea = groupAreaRange[[1]],
                                              fn_maxArea = groupAreaRange[[2]],
                                              fn_minRoundness = groupRoundnessRange[[1]],
                                              fn_maxRoundness = groupRoundnessRange[[2]],
                                              fn_seedOutScore = mthdPrmtrs$seedOutScore,
                                              fn_cutSeedList = mthdPrmtrs$cutSeedList,
                                              fn_cycleWindow = mthdPrmtrs$cycleWindow,
                                              fn_discoverTreshold = mthdPrmtrs$discoverTreshold,
                                              fn_adaptative = mthdPrmtrs$adaptative,
                                              fn_drastic = groupAreaRange[[1]]*mthdPrmtrs$drastic,
                                              fn_direction = mthdPrmtrs$direction,
                                              fn_seed = mthdPrmtrs$seed))
                         polygonsList[[rst]][[i]]<-list()
                         polygonsList[[rst]][[i]]<-TEMP[[1]]


                       }
                     }
                   },

                   ratMap = {

                     rstToSegment<-sapply(names(x$currentAnalysis$classification),function(nms){
                       if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) stop(RUNIMC:::mError('Check classification layer name provided'))
                       return(x$currentAnalysis$classification[[nms]][[labelLayer]])
                     },USE.NAMES = T,simplify = F)

                     polygonsList<-list()
                     for (rst in names(rstToSegment)){
                       for (i in labelLayer){

                         mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                         mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,i),USE.NAMES = F,simplify = T))


                         cat(paste(rst,mrkr[mrkrIndex],'\n',sep=":::"))
                         group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                         group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                         groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)*mthdPrmtrs$areaExpansion
                         groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)*mthdPrmtrs$roundnessExpansion

                         TEMP<-list(ratMap(fn_srt = rstToSegment[[rst]][[i]],
                                           fn_Nspikes=mthdPrmtrs$spikes,
                                           fn_radius = round(sqrt(groupAreaRange[[2]]*mthdPrmtrs$radiusExpansion/pi)),
                                           fn_coverage = mthdPrmtrs$coverage,
                                           fn_minArea = groupAreaRange[[1]],
                                           fn_maxArea = groupAreaRange[[2]],
                                           fn_minRoundness = groupRoundnessRange[[1]],
                                           fn_maxRoundness = groupRoundnessRange[[2]],
                                           fn_seedOutScore = mthdPrmtrs$seedOutScore,
                                           fn_cycleWindow = mthdPrmtrs$cycleWindow,
                                           fn_discoverTreshold = mthdPrmtrs$discoverTreshold,
                                           fn_adaptative = mthdPrmtrs$adaptative,
                                           fn_drastic = groupAreaRange[[1]]*mthdPrmtrs$drastic,
                                           fn_lowPenalty = mthdPrmtrs$lowPenalty,
                                           fn_highPenalty = mthdPrmtrs$highPenalty,
                                           fn_roundnessPenalty = mthdPrmtrs$roundnessPenalty,
                                           fn_seed = mthdPrmtrs$seed))
                         polygonsList[[rst]][[i]]<-list()
                         polygonsList[[rst]][[i]]<-TEMP[[1]]

                       }
                     }
                   },

                   slothMap = {


                     rstToSegment<-sapply(names(x$currentAnalysis$classification),function(nms){
                       if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) stop(RUNIMC:::mError('Check classification layer name provided'))
                       return(x$currentAnalysis$classification[[nms]][[labelLayer]])
                     },USE.NAMES = T,simplify = F)

                     polygonsList<-list()
                     for (rst in names(rstToSegment)){
                       for (i in labelLayer){

                         mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                         mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,i),USE.NAMES = F,simplify = T))


                         cat(paste(rst,mrkr[mrkrIndex],'\n',sep=":::"))
                         group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                         group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                         groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)
                         groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)

                         if (is.character(mthdPrmtrs$targetArea)){
                           switch(mthdPrmtrs$targetArea,
                                  training_mean = {
                                    targetArea<-mean(unlist(group_area))
                                  },
                                  training_median = {
                                    targetArea<-median(unlist(group_area))
                                  },
                                  training_mode = {
                                    brks<-0:ceiling(unlist(group_area))
                                    frqT<-hist(x = unlist(group_area),breaks = brks)
                                    targetArea<-median(frqT$breaks[which(frqT$counts==max(frqT$counts))])
                                  },
                                  training_max = {
                                    targetArea<-max(unlist(group_area))
                                  },
                                  training_middle = {
                                    targetArea<-(max(unlist(group_area))-min(unlist(group_area)))/2
                                  },
                                  {targetArea<-mthdPrmtrs$targetArea})
                         } else {targetArea<-mthdPrmtrs$targetArea}


                         TEMP<-list(slothMap(fn_srt = rstToSegment[[rst]][[i]],
                                             fn_Nspikes=mthdPrmtrs$spikes,
                                             fn_radius = round(sqrt(groupAreaRange[[2]]*mthdPrmtrs$radiusExpansion/pi)),
                                             fn_coverage = mthdPrmtrs$coverage,
                                             fn_minArea = groupAreaRange[[1]],
                                             fn_maxArea = groupAreaRange[[2]],
                                             fn_minRoundness = groupRoundnessRange[[1]],
                                             fn_maxRoundness = groupRoundnessRange[[2]],
                                             fn_seedOutScore = mthdPrmtrs$seedOutScore,
                                             fn_cycleWindow = mthdPrmtrs$cycleWindow,
                                             fn_discoverTreshold = mthdPrmtrs$discoverTreshold,
                                             fn_adaptative = mthdPrmtrs$adaptative,
                                             fn_areaAdaptRate = mthdPrmtrs$areaAdaptRate,
                                             fn_roundnessAdaptRate = mthdPrmtrs$roundnessAdaptRate,
                                             fn_fusion = mthdPrmtrs$fusion,
                                             fn_maxNetworkSize = mthdPrmtrs$maxNetworkSize,
                                             fn_targetArea = targetArea,
                                             fn_inflateDeflate = mthdPrmtrs$inflateDeflate,
                                             fn_favourForeing = mthdPrmtrs$favourForeing,
                                             fn_returnKinetic = mthdPrmtrs$returnKinetic,
                                             fn_returnRasters = mthdPrmtrs$returnRasters))

                         polygonsList[[rst]][[i]]<-list()
                         polygonsList[[rst]][[i]]<-TEMP[[1]]

                       }
                     }
                   },

                   alligatorMap = {


                     rstToSegment<-sapply(names(x$currentAnalysis$classification),function(nms){
                       if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) stop(RUNIMC:::mError('Check classification layer name provided'))
                       return(x$currentAnalysis$classification[[nms]][[labelLayer]])
                     },USE.NAMES = T,simplify = F)

                     polygonsList<-list()
                     for (rst in names(rstToSegment)){
                       for (i in labelLayer){

                         mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                         mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,i),USE.NAMES = F,simplify = T))


                         cat(paste(rst,mrkr[mrkrIndex],'\n',sep=":::"))
                         group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                         group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                         groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)
                         groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)

                         if (is.character(mthdPrmtrs$targetArea)){
                           switch(mthdPrmtrs$targetArea,
                                  training_mean = {
                                    targetArea<-mean(unlist(group_area))
                                  },
                                  training_median = {
                                    targetArea<-median(unlist(group_area))
                                  },
                                  training_mode = {
                                    brks<-0:ceiling(unlist(group_area))
                                    frqT<-hist(x = unlist(group_area),breaks = brks)
                                    targetArea<-median(frqT$breaks[which(frqT$counts==max(frqT$counts))])
                                  },
                                  training_max = {
                                    targetArea<-max(unlist(group_area))
                                  },
                                  training_middle = {
                                    targetArea<-(max(unlist(group_area))-min(unlist(group_area)))/2
                                  },
                                  {targetArea<-mthdPrmtrs$targetArea})
                         } else {targetArea<-mthdPrmtrs$targetArea}


                         TEMP<-list(alligatorMap(fn_srt = rstToSegment[[rst]][[i]],
                                                 fn_Nspikes=mthdPrmtrs$spikes,
                                                 fn_radius = round(sqrt(groupAreaRange[[2]]*mthdPrmtrs$radiusExpansion/pi)),
                                                 fn_coverage = mthdPrmtrs$coverage,
                                                 fn_minArea = groupAreaRange[[1]],
                                                 fn_maxArea = groupAreaRange[[2]],
                                                 fn_minRoundness = groupRoundnessRange[[1]],
                                                 fn_maxRoundness = groupRoundnessRange[[2]],
                                                 fn_seedOutScore = mthdPrmtrs$seedOutScore,
                                                 fn_cycleWindow = mthdPrmtrs$cycleWindow,
                                                 fn_adaptative = mthdPrmtrs$adaptative,
                                                 fn_areaAdaptRate = mthdPrmtrs$areaAdaptRate,
                                                 fn_roundnessAdaptRate = mthdPrmtrs$roundnessAdaptRate,
                                                 fn_segmentAlg = mthdPrmtrs$segmentAlg,
                                                 fn_fusion = mthdPrmtrs$fusion,
                                                 fn_maxNetworkSize = mthdPrmtrs$maxNetworkSize,
                                                 fn_targetArea = targetArea,
                                                 fn_inflateDeflate = mthdPrmtrs$inflateDeflate,
                                                 fn_favourForeing = mthdPrmtrs$favourForeing,
                                                 fn_returnKinetic = mthdPrmtrs$returnKinetic,
                                                 fn_returnRasters = mthdPrmtrs$returnRasters))

                         polygonsList[[rst]][[i]]<-list()
                         polygonsList[[rst]][[i]]<-TEMP[[1]]

                       }
                     }
                   },

                   lazyCatMap = {

                     rstToSegment<-sapply(names(x$currentAnalysis$classification),function(nms){
                       if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) stop(RUNIMC:::mError('Check classification layer name provided'))
                       return(x$currentAnalysis$classification[[nms]][[labelLayer]])
                     },USE.NAMES = T,simplify = F)

                     polygonsList<-list()
                     for (rst in names(rstToSegment)){
                       for (i in labelLayer){

                         cat(paste(rst,i,'\n',sep=":::"))

                         TEMP<-list(lazyCatMap_NEW(fn_srt = rstToSegment[[rst]][[i]],
                                                   fn_indexToExclude = mthdPrmtrs$indexToExclude))
                         polygonsList[[rst]][[i]]<-list()
                         polygonsList[[rst]][[i]]<-TEMP[[1]]

                       }
                     }
                   },

                   pandaMap = {

                     rstToSegment<-sapply(names(x$currentAnalysis$classification),function(nms){
                       if (!any(labelLayer %in% names(x$currentAnalysis$classification[[nms]]))) stop(RUNIMC:::mError('Check classification layer name provided'))
                       return(x$currentAnalysis$classification[[nms]][[labelLayer]])
                     },USE.NAMES = T,simplify = F)

                     polygonsList<-sapply(names(rstToSegment),function(x){
                       sapply(labelLayer,function(x){},
                              simplify = F,USE.NAMES = T)},
                       simplify = F,USE.NAMES = T)

                     for (rst in names(rstToSegment)){
                       for (i in labelLayer){

                         mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                         mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,i),USE.NAMES = F,simplify = T))


                         cat(paste(rst,mrkr[mrkrIndex],'\n',sep=":::"))

                         polygonsList[[rst]][[i]]<-pandaMap(fn_srt = rstToSegment[[rst]][[i]],
                                                            fn_uid = rst,
                                                            fn_primaryIndex=mrkr[mrkrIndex],
                                                            fn_clpDir = mthdPrmtrs$ClampDetectionDirection,
                                                            fn_brake = mthdPrmtrs$nOfCutBrakes,
                                                            fn_lowerQuantile = mthdPrmtrs$lowerQuantile,
                                                            fn_upperQuantile = mthdPrmtrs$upperQuantile,
                                                            fn_lowerAreaLimit= mthdPrmtrs$lowerAreaLimit,
                                                            fn_movingWindow_dim = mthdPrmtrs$movingWindowDimension,
                                                            fn_movingWindow_overlap = mthdPrmtrs$overlapExtent,
                                                            fn_cores = mthdPrmtrs$numberOfCores,
                                                            fn_verbose = mthdPrmtrs$verbose)

                       }
                     }


                     polygonsList<-lapply(polygonsList,function(xuid){
                       do.call(rbind.data.frame,append(xuid,
                                                       list(make.row.names = F,
                                                            stringsAsFactors = F,
                                                            deparse.level=0)
                       )
                       )
                     })

                     polygonsList<-do.call(rbind.data.frame,append(polygonsList,
                                                                   list(make.row.names = F,
                                                                        stringsAsFactors = F,
                                                                        deparse.level=0)))

                     if (mthdPrmtrs$distillDirect){

                       condensedPoligonList<-extractMeanPixel(fn_polygons = polygonsList,
                                                              fn_raster = x$raster)
                     }

                       newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                       condensedPoligonList<-initObjectAttr(condensedPoligonList)

                       x$currentAnalysis$exprs<-condensedPoligonList
                       attr(x,'mdtnTimeStmp')<-newTimeStmp
                       attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
                       return()

                   }
            )


            polygonsList<-new('IMC_SegmentationList',polygonsList)
            x$currentAnalysis$segmentation<-polygonsList

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
          })


#** testSegment ---------------------------------------------------

if (!isGeneric("testSegment")) {
  setGeneric("testSegment", function(x,labelLayer='label',label=NULL,uid=NULL,limits=NULL,...)
    standardGeneric("testSegment"))
}

#'
#'
#' @export
setMethod('testSegment',signature = ('environment'),
          function(x,labelLayer='label',label=NULL,uid=NULL,limits=NULL,...){



            if (is.null(x$currentAnalysis$segmentationDirectives)) stop(mError('Before segmenting directives must be specified'))
            if (is.null(labelLayer)) stop(mError('specify the classification layer to segment'))
            if (is.null(label)) stop(mError('specify the classification label to segment'))
            if (is.null(uid)) stop(mError('specify sample(uid) to segment'))



            avlblUids<-names(x$currentAnalysis$classification)
            if (!any(uid %in% avlblUids)) {stop(paste0("couldn't find ",uid,". Available samples(uid) are: ",paste0(avlblUids,collapse='/n')))}

            avlblLabelLayers<-names(x$currentAnalysis$classification[[uid]])
            if (!any(labelLayer %in% avlblLabelLayers)) {stop(paste0("couldn't find ",labelLayer,". Available labelLayers are: ",paste0(avlblLabelLayers,collapse=', ')))}

            if (is.null(limits)){
              message(mWarning('limits not specified, the entire raster will be used'))
              limits<-raster::extent(x$currentAnalysis$classification[[uid]][[labelLayer]])}



            mthd<-x$currentAnalysis$segmentationDirectives@method
            mthdPrmtrs<-x$currentAnalysis$segmentationDirectives@methodParameters

            switch(mthd,

                   spiderMap = {

                     rstToSegment<-raster::crop(x=x$currentAnalysis$classification[[uid]][[labelLayer]],y=limits)

                     iMat<-x$currentAnalysis$interpretationMatrix


                     cat(paste(uid,label,'\n',sep=":::"))
                     interpretationMatrixInstance<-iMat[[label]]
                     group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==label]
                     group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==label]
                     groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)*mthdPrmtrs$areaExpansion
                     groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)*mthdPrmtrs$roundnessExpansion

                     timerStart<-Sys.time()

                     TEMP<-list(spiderMap(fn_srt = rstToSegment,
                                          fn_interpret = interpretationMatrixInstance,
                                          fn_Nspikes=mthdPrmtrs$spikes,
                                          fn_radius = round(sqrt(groupAreaRange[[2]]*mthdPrmtrs$radiusExpansion/pi)),
                                          fn_densityMultiplier=mthdPrmtrs$densityMultiplier,
                                          fn_coverage = mthdPrmtrs$coverage,
                                          fn_minArea = groupAreaRange[[1]],
                                          fn_maxArea = groupAreaRange[[2]],
                                          fn_minRoundness = groupRoundnessRange[[1]],
                                          fn_maxRoundness = groupRoundnessRange[[2]],
                                          fn_seedOutScore = mthdPrmtrs$seedOutScore,
                                          fn_cutSeedList = mthdPrmtrs$cutSeedList,
                                          fn_cycleWindow = mthdPrmtrs$cycleWindow,
                                          fn_discoverTreshold = mthdPrmtrs$discoverTreshold,
                                          fn_adaptative = mthdPrmtrs$adaptative,
                                          fn_drastic = groupAreaRange[[1]]*mthdPrmtrs$drastic,
                                          fn_direction = mthdPrmtrs$direction,
                                          fn_seed = mthdPrmtrs$seed))

                     timerStop<-Sys.time()

                     polygonsList<-list()
                     polygonsList[[uid]][[label]]<-list()
                     polygonsList[[uid]][[label]]<-TEMP[[1]]

                     newMarker<-label
                     dumpMarkers<-interpretationMatrixInstance$label[interpretationMatrixInstance$level!=label]
                     rasterMask<-lapply(dumpMarkers, function(dmpM){
                       polygonsList[[uid]][[label]]@raster==dmpM
                     })
                     rasterMask<-raster::stack(rasterMask)
                     rasterMask<-raster::calc(rasterMask,sum)
                     rasterMask<-rasterMask==0
                     polygonsList[[uid]][[label]]@raster<-raster::mask(polygonsList[[uid]][[label]]@raster,rasterMask,maskvalue=0,updateValue=NA)
                   },

                   ratMap = {

                     rstToSegment<-raster::crop(x=x$currentAnalysis$classification[[uid]][[labelLayer]],y=limits)

                     mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                     mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,label),USE.NAMES = F,simplify = T))

                     cat(paste(uid,mrkr[mrkrIndex],'\n',sep=":::"))
                     group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                     group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                     groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)*mthdPrmtrs$areaExpansion
                     groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)*mthdPrmtrs$roundnessExpansion

                     timerStart<-Sys.time()

                     TEMP<-list(ratMap(fn_srt = rstToSegment,
                                       fn_Nspikes=mthdPrmtrs$spikes,
                                       fn_radius = round(sqrt(groupAreaRange[[2]]*mthdPrmtrs$radiusExpansion/pi)),
                                       fn_coverage = mthdPrmtrs$coverage,
                                       fn_minArea = groupAreaRange[[1]],
                                       fn_maxArea = groupAreaRange[[2]],
                                       fn_minRoundness = groupRoundnessRange[[1]],
                                       fn_maxRoundness = groupRoundnessRange[[2]],
                                       fn_seedOutScore = mthdPrmtrs$seedOutScore,
                                       fn_cycleWindow = mthdPrmtrs$cycleWindow,
                                       fn_discoverTreshold = mthdPrmtrs$discoverTreshold,
                                       fn_adaptative = mthdPrmtrs$adaptative,
                                       fn_drastic = groupAreaRange[[1]]*mthdPrmtrs$drastic,
                                       fn_lowPenalty = mthdPrmtrs$lowPenalty,
                                       fn_highPenalty = mthdPrmtrs$highPenalty,
                                       fn_roundnessPenalty = mthdPrmtrs$roundnessPenalty,
                                       fn_seed = mthdPrmtrs$seed))

                     timerStop<-Sys.time()


                     polygonsList<-list()
                     polygonsList[[uid]][[mrkr[mrkrIndex]]]<-list()
                     polygonsList[[uid]][[mrkr[mrkrIndex]]]<-TEMP[[1]]

                     newMarker<-mrkr[mrkrIndex]

                   },

                   slothMap = {

                     rstToSegment<-raster::crop(x=x$currentAnalysis$classification[[uid]][[labelLayer]],y=limits)

                     mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                     mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,label),USE.NAMES = F,simplify = T))

                     cat(paste(uid,mrkr[mrkrIndex],'\n',sep=":::"))
                     group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                     group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                     groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)
                     groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)

                     if (is.character(mthdPrmtrs$targetArea)){
                       switch(mthdPrmtrs$targetArea,
                              training_mean = {
                                targetArea<-mean(unlist(group_area))
                              },
                              training_median = {
                                targetArea<-median(unlist(group_area))
                              },
                              training_mode = {
                                brks<-0:ceiling(unlist(group_area))
                                frqT<-hist(x = unlist(group_area),breaks = brks)
                                targetArea<-median(frqT$breaks[which(frqT$counts==max(frqT$counts))])
                              },
                              training_max = {
                                targetArea<-max(unlist(group_area))
                              },
                              training_middle = {
                                targetArea<-(max(unlist(group_area))-min(unlist(group_area)))/2
                              },
                              {targetArea<-mthdPrmtrs$targetArea})
                     } else {targetArea<-mthdPrmtrs$targetArea}
                     timerStart<-Sys.time()

                     TEMP<-list(slothMap(fn_srt = rstToSegment,
                                         fn_Nspikes=mthdPrmtrs$spikes,
                                         fn_radius = round(sqrt(groupAreaRange[[2]]*mthdPrmtrs$radiusExpansion/pi)),
                                         fn_coverage = mthdPrmtrs$coverage,
                                         fn_minArea = groupAreaRange[[1]],
                                         fn_maxArea = groupAreaRange[[2]],
                                         fn_minRoundness = groupRoundnessRange[[1]],
                                         fn_maxRoundness = groupRoundnessRange[[2]],
                                         fn_seedOutScore = mthdPrmtrs$seedOutScore,
                                         fn_cycleWindow = mthdPrmtrs$cycleWindow,
                                         fn_discoverTreshold = mthdPrmtrs$discoverTreshold,
                                         fn_adaptative = mthdPrmtrs$adaptative,
                                         fn_areaAdaptRate = mthdPrmtrs$areaAdaptRate,
                                         fn_roundnessAdaptRate = mthdPrmtrs$roundnessAdaptRate,
                                         fn_fusion = mthdPrmtrs$fusion,
                                         fn_maxNetworkSize = mthdPrmtrs$maxNetworkSize,
                                         fn_targetArea = targetArea,
                                         fn_inflateDeflate = mthdPrmtrs$inflateDeflate,
                                         fn_favourForeing = mthdPrmtrs$favourForeing,
                                         fn_returnKinetic = T,
                                         fn_returnRasters = T))

                     timerStop<-Sys.time()


                     polygonsList<-list()
                     polygonsList[[uid]][[mrkr[mrkrIndex]]]<-list()
                     polygonsList[[uid]][[mrkr[mrkrIndex]]]<-TEMP[[1]]

                     newMarker<-mrkr[mrkrIndex]

                   },

                   alligatorMap = {

                     rstToSegment<-raster::crop(x=x$currentAnalysis$classification[[uid]][[labelLayer]],y=limits)

                     mrkr<-tf_labelList(x$currentAnalysis$trainingFeatures)
                     mrkrIndex<-which(sapply(mrkr,function(x)grepl(x,label),USE.NAMES = F,simplify = T))

                     cat(paste(uid,mrkr[mrkrIndex],'\n',sep=":::"))
                     group_area<-x$currentAnalysis$trainingFeatures$geometry$area[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                     group_roundness<-x$currentAnalysis$trainingFeatures$geometry$roundness[x$currentAnalysis$trainingFeatures$geometry$label==mrkr[mrkrIndex]]
                     groupAreaRange<-quantile(group_area,mthdPrmtrs$areaQuantile)
                     groupRoundnessRange<-quantile(group_roundness,mthdPrmtrs$roundnessQuantile)

                     if (is.character(mthdPrmtrs$targetArea)){
                       switch(mthdPrmtrs$targetArea,
                              training_mean = {
                                targetArea<-mean(unlist(group_area))
                              },
                              training_median = {
                                targetArea<-median(unlist(group_area))
                              },
                              training_mode = {
                                brks<-0:ceiling(unlist(group_area))
                                frqT<-hist(x = unlist(group_area),breaks = brks)
                                targetArea<-median(frqT$breaks[which(frqT$counts==max(frqT$counts))])
                              },
                              training_max = {
                                targetArea<-max(unlist(group_area))
                              },
                              training_middle = {
                                targetArea<-(max(unlist(group_area))-min(unlist(group_area)))/2
                              },
                              {targetArea<-mthdPrmtrs$targetArea})
                     } else {targetArea<-mthdPrmtrs$targetArea}
                     timerStart<-Sys.time()

                     TEMP<-list(alligatorMap(fn_srt = rstToSegment,
                                             fn_Nspikes=mthdPrmtrs$spikes,
                                             fn_radius = round(sqrt(groupAreaRange[[2]]*mthdPrmtrs$radiusExpansion/pi)),
                                             fn_coverage = mthdPrmtrs$coverage,
                                             fn_minArea = groupAreaRange[[1]],
                                             fn_maxArea = groupAreaRange[[2]],
                                             fn_minRoundness = groupRoundnessRange[[1]],
                                             fn_maxRoundness = groupRoundnessRange[[2]],
                                             fn_seedOutScore = mthdPrmtrs$seedOutScore,
                                             fn_cycleWindow = mthdPrmtrs$cycleWindow,
                                             fn_adaptative = mthdPrmtrs$adaptative,
                                             fn_areaAdaptRate = mthdPrmtrs$areaAdaptRate,
                                             fn_roundnessAdaptRate = mthdPrmtrs$roundnessAdaptRate,
                                             fn_segmentAlg = mthdPrmtrs$segmentAlg,
                                             fn_fusion = mthdPrmtrs$fusion,
                                             fn_maxNetworkSize = mthdPrmtrs$maxNetworkSize,
                                             fn_targetArea = targetArea,
                                             fn_inflateDeflate = mthdPrmtrs$inflateDeflate,
                                             fn_favourForeing = mthdPrmtrs$favourForeing,
                                             fn_returnKinetic = T,
                                             fn_returnRasters = T))

                     timerStop<-Sys.time()


                     polygonsList<-list()
                     polygonsList[[uid]][[mrkr[mrkrIndex]]]<-list()
                     polygonsList[[uid]][[mrkr[mrkrIndex]]]<-TEMP[[1]]

                     newMarker<-mrkr[mrkrIndex]

                   }
            )


            polygonsList<-new('IMC_SegmentationList',polygonsList)

            condensedPoligonList<-extractPolygons(polygonsList)
            condensedPoligonList<-extractMeanPixel(fn_polygons = condensedPoligonList,
                                                   fn_raster = x$raster)


            logicRaster<-polygonsList[[uid]][[newMarker]]@raster<0

            positiveCells<-raster::freq(logicRaster,
                                        digits=3,
                                        progress='text')
            totalCells<-sum(positiveCells[,'count'])
            labelCells<-sum(positiveCells[!is.na(positiveCells[,'value']),'count'])
            ratioTotal<-positiveCells[,'count']/totalCells
            ratioLabel<-positiveCells[,'count']/labelCells
            positiveCells<-cbind(positiveCells,ratioTotal,ratioLabel)
            colnames(positiveCells)<-c('label','count','POT','POL')

            out_PL<-as.data.frame(sf::st_drop_geometry(condensedPoligonList[,!(names(condensedPoligonList) %in% c('uid','polygon.id','label','color','GEOMETRY'))]))
            out_PL<-summary(out_PL)
            out_GM<-sf::st_geometry(condensedPoligonList)
            out_STK<-raster::stack(list(original=rstToSegment,
                                        segmented=polygonsList[[uid]][[newMarker]]@raster,
                                        logical=logicRaster,
                                        data=raster::crop(x=x$raster[[uid]],y=limits)))

            out_timer<-data.frame(start=timerStart,
                                  stop=timerStop,
                                  duration=(timerStop-timerStart),
                                  expected=(x$currentAnalysis$classification[[uid]][[labelLayer]]@ncols*
                                              x$currentAnalysis$classification[[uid]][[labelLayer]]@nrows)/
                                    (rstToSegment@ncols*rstToSegment@nrows)*(timerStop-timerStart))

            out<-list(timer =out_timer,
                      summary = out_PL,
                      coverage = positiveCells,
                      geometry = out_GM,
                      raster =out_STK)



            return(out)
          })



#** distilExpression ---------------------------------------------------

if (!isGeneric("distilExpression")) {
  setGeneric("distilExpression", function(x,method='weightedMean',...)
    standardGeneric("distilExpression"))
}

#' @export
setMethod('distilExpression',signature = ('environment'),
          function(x,method='weightedMean',...){


            if (is.null(x$currentAnalysis$segmentation)) stop(mError('could not find any segmentation'))
            if (is.null(x$raster)) stop(mError('could not find any raster to process for segmentation'))

            condensedPoligonList<-extractPolygons(x$currentAnalysis$segmentation)
            condensedPoligonList<-extractMeanPixel(fn_polygons = condensedPoligonList,
                                                   fn_raster = x$raster)

            # condensedPoligonList<-as(condensedPoligonList,'IMC_ExpressionMatrix')
            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            condensedPoligonList<-initObjectAttr(condensedPoligonList)

            x$currentAnalysis$exprs<-condensedPoligonList
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
          })
#
# #** archive ---------------------------------------------------
#
# if (!isGeneric("archive")) {
#   setGeneric("archive", function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,studyTable=NULL,...)
#     standardGeneric("archive"))
# }
#
#
# setMethod('archive',signature = ('environment'),
#           function(x,what=NULL,objectReturn=F,forceSave=F,studyTable=NULL,...){
#
#             objectList<-ls(x)
#
#             if (any('rootFolder' %in% objectList)){
#
#
#               if (!forceSave){
#                 mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#                 artnTimeStmp<-attr(x,'artnTimeStmp')
#                 if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                   if (mdtnTimeStmp==artnTimeStmp){
#                     message(mWarning('Archived study is up to date, you might use "forceSave=T"'))
#                     return(0)
#                   }
#                 }
#               }
#
#               basePath<-paste(x$rootFolder,x$name,sep='/')
#
#               #### study table
#               outF<-archive(x = x$studyTable,
#                             filePathName = paste(basePath,'st',sep='/'),
#                             objectReturn = F,
#                             forceSave = forceSave)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$studyTable,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$studyTable,'artnTimeStmp')<-newTimeStmp
#                 attr(x$studyTable,'fileArchive')<-outF
#                 message(mMessage('Study table archivation... OK'))
#               }
#               #### channel table
#               outF<-archive(x = x$channels,
#                             filePathName = paste(basePath,'ch',sep='/'),
#                             objectReturn = F,
#                             forceSave = forceSave)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$channels,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$channels,'artnTimeStmp')<-newTimeStmp
#                 attr(x$channels,'fileArchive')<-outF
#                 message(mMessage('Channel table archivation... OK'))
#               }
#               #### rasters
#
#               outF<-archive(x = x$raster,
#                             filePathName = paste(basePath),
#                             objectReturn = F,
#                             forceSave = forceSave,
#                             studyTable=x$studyTable)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$raster,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$raster,'artnTimeStmp')<-newTimeStmp
#                 attr(x$raster,'fileArchive')<-outF
#                 message(mMessage('Raster archivation... OK'))
#               }
#               #### current analysis
#               outF<-archive(x = x$currentAnalysis,
#                             filePathName = paste(basePath,'Analysis',sep='/'),
#                             objectReturn = F,
#                             forceSave = forceSave,
#                             studyTable = x$studyTable)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$currentAnalysis,'artnTimeStmp')<-newTimeStmp
#                 attr(x$currentAnalysis,'fileArchive')<-outF
#                 message(mMessage('Current analysis archivation... OK'))
#               }
#
#               writePermit<-file.access(paste0(basePath,'/study.xml'), mode=2)
#               fileExists<-file.exists(paste0(basePath,'/study.xml'))
#               if (writePermit==0 | !fileExists){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x,'artnTimeStmp')<-newTimeStmp
#                 attr(x,'fileArchive')<-paste0(basePath,'/study.xml')
#                 xmlOut<-XMLparseObject(x,'study')
#                 XML::saveXML(xmlOut,paste0(basePath,'/study.xml'))
#               } else {stop(RUNIMC:::mError(paste0('Write permission denied to: ',basePath,'/study.xml')))}
#
#               if (objectReturn){
#
#                 return(x)
#               }
#               return(paste0(basePath,'/study.xml'))
#             }
#
#             #### current analysis
#             if (any('folder' %in% objectList)){
#
#
#               if (!forceSave){
#                 mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#                 artnTimeStmp<-attr(x,'artnTimeStmp')
#                 if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                   if (mdtnTimeStmp==artnTimeStmp){
#                     message(mWarning('Archived study is up to date, you might use "forceSave=T"'))
#                     return(0)
#                   }
#                 }
#               }
#
#               basePath<-x$folder
#
#               #### classification
#               outF<-archive(x = x$classification,
#                             filePathName = file.path(basePath,'/test/classification'),
#                             objectReturn = F,
#                             forceSave = forceSave,
#                             studyTable = studyTable)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$classification,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$classification,'artnTimeStmp')<-newTimeStmp
#                 attr(x$classification,'fileArchive')<-outF
#                 message(mMessage('Classification archivation... OK'))
#               }
#               #### classificationDirectives
#               outF<-archive(x = x$classificationDirectives,
#                             filePathName = paste(basePath,'cd',sep='/'),
#                             objectReturn = F,
#                             forceSave = forceSave)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$classificationDirectives,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$classificationDirectives,'artnTimeStmp')<-newTimeStmp
#                 attr(x$classificationDirectives,'fileArchive')<-outF
#                 message(mMessage('Classification directives archivation... OK'))
#               }
#               #### classifier
#               outF<-archive(x = x$classifier,
#                             filePathName = paste(basePath,'cs',sep='/'),
#                             objectReturn = F,
#                             forceSave = forceSave)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$classifier,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$classifier,'artnTimeStmp')<-newTimeStmp
#                 attr(x$classifier,'fileArchive')<-outF
#                 message(mMessage('Classifier archivation... OK'))
#               }
#               #### derivedRasters
#               outF<-archive(x = x$derivedRasters,
#                             filePathName = basePath,
#                             objectReturn = F,
#                             forceSave = forceSave,
#                             studyTable=parent.env(x)$studyTable)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$derivedRasters,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$derivedRasters,'artnTimeStmp')<-newTimeStmp
#                 attr(x$derivedRasters,'fileArchive')<-outF
#                 message(mMessage('Derived raster archivation... OK'))
#               }
#               #### exprs
#               outF<-archive(x = x$exprs,
#                             filePathName = paste(basePath,'ex',sep='/'),
#                             objectReturn = F,
#                             forceSave = forceSave)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$exprs,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$exprs,'artnTimeStmp')<-newTimeStmp
#                 attr(x$exprs,'fileArchive')<-outF
#                 message(mMessage('Expression table archivation... OK'))
#               }
#               #### extractionDirectives
#               outF<-archive(x = x$extractionDirectives,
#                             filePathName = paste(basePath,'ed',sep='/'),
#                             objectReturn = F,
#                             forceSave = forceSave)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$extractionDirectives,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$extractionDirectives,'artnTimeStmp')<-newTimeStmp
#                 attr(x$extractionDirectives,'fileArchive')<-outF
#                 message(mMessage('Extraction directives archivation... OK'))
#               }
#               #### filters
#               outF<-archive(x = x$filters,
#                             filePathName = paste(basePath,'fl',sep='/'),
#                             objectReturn = F,
#                             forceSave = forceSave)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$filters,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$filters,'artnTimeStmp')<-newTimeStmp
#                 attr(x$filters,'fileArchive')<-outF
#                 message(mMessage('Filter archivation... OK'))
#               }
#               #### interpretationMatrix
#               outF<-archive(x = x$interpretationMatrix,
#                             filePathName = paste(basePath,'im',sep='/'),
#                             objectReturn = F,
#                             forceSave = forceSave)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$interpretationMatrix,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$interpretationMatrix,'artnTimeStmp')<-newTimeStmp
#                 attr(x$interpretationMatrix,'fileArchive')<-outF
#                 message(mMessage('Interpretation matrix archivation... OK'))
#               }
#               #### segmentation
#               outF<-archive(x = x$segmentation,
#                             filePathName = paste(basePath,'sg',sep='/'),
#                             objectReturn = F,
#                             forceSave = forceSave)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$segmentation,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$segmentation,'artnTimeStmp')<-newTimeStmp
#                 attr(x$segmentation,'fileArchive')<-outF
#                 message(mMessage('Segmentation archivation... OK'))
#               }
#               #### segmentationDirectives
#               outF<-archive(x = x$segmentationDirectives,
#                             filePathName = paste(basePath,'sd',sep='/'),
#                             objectReturn = F,
#                             forceSave = forceSave)
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$segmentationDirectives,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$segmentationDirectives,'artnTimeStmp')<-newTimeStmp
#                 attr(x$segmentationDirectives,'fileArchive')<-outF
#                 message(mMessage('Segmentation directives archivation... OK'))
#               }
#               #### trainingFeatures
#               outF<-archive(x = x$trainingFeatures,
#                             filePathName = paste(basePath,'tf',sep='/'),
#                             objectReturn = F,
#                             forceSave = forceSave,
#               )
#               if (outF!=-1 & outF!=0){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x$trainingFeatures,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x$trainingFeatures,'artnTimeStmp')<-newTimeStmp
#                 attr(x$trainingFeatures,'fileArchive')<-outF
#                 message(mMessage('Training features archivation... OK'))
#               }
#
#               writePermit<-file.access(paste0(basePath,'/analysis.xml'), mode=2)
#               fileExists<-file.exists(paste0(basePath,'/analysis.xml'))
#               if (writePermit==0 | !fileExists){
#                 newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
#                 attr(x,'mdtnTimeStmp')<-newTimeStmp
#                 attr(x,'artnTimeStmp')<-newTimeStmp
#                 attr(x,'fileArchive')<-paste0(basePath,'/analysis.xml')
#                 xmlOut<-XMLparseObject(x,'analysis')
#                 XML::saveXML(xmlOut,paste0(basePath,'/analysis.xml'))
#               } else {stop(RUNIMC:::mError(paste0('Write permission denied to: ',basePath,'/analysis.xml')))}
#
#               if (objectReturn){
#
#                 return(x)
#               }
#               return(paste0(basePath,'/analysis.xml'))
#
#             }
#
#
#           })
#
# #*** archive:imc_studyTable ---------------------------------------------------
#
# setMethod('archive',signature = 'IMC_StudyTable',
#           function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,...){
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             if (!endsWith(filePathName,'.IMC_StudyTable')) filePathName<-paste0(filePathName,'.IMC_StudyTable')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('Study table file is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             fileOK<-try(utils::write.table(x,filePathName,row.names = F,quote = F,sep = '\t'))
#
#             if (exists('fileOK')){
#               if (!is.null(fileOK)){
#                 if (inherits(fileOK,what = 'try-error')){
#                   message(mError('Study table file archiviation failed'))
#                   return(-1)
#                 }
#               }
#             }
#             if (objectReturn){
#               return(x)
#             }
#             return(filePathName)
#           })
#
#
# #*** archive:imc_channeltable ---------------------------------------------------
#
# setMethod('archive',signature = 'IMC_ChannelTable',
#           function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,...){
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             if (!endsWith(filePathName,'.IMC_ChannelTable')) filePathName<-paste0(filePathName,'.IMC_ChannelTable')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('Channel table file is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             fileOK<-try(utils::write.table(x,filePathName,row.names = F,quote = F,sep = '\t'))
#
#             if (exists('fileOK')){
#               if (!is.null(fileOK)){
#                 if (inherits(fileOK,what = 'try-error')){
#                   message(mError('Channel table file archiviation failed'))
#                   return(-1)
#                 }
#               }
#             }
#             if (objectReturn){
#               return(x)
#             }
#             return(filePathName)
#           })
#
#
# #*** archive:imc_RsCollection ---------------------------------------------------
#
# setMethod('archive',signature = c('IMC_RsCollection'),
#           function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,studyTable=NULL,...){
#
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             # if (!endsWith(filePathName,'.txt')) filePathName<-paste0(filePathName,'.txt')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('RasterStack collection is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             dir.create(filePathName,showWarnings = F)
#
#             rstrStk<-sapply(names(x),function(smp){
#
#               if (is.null(studyTable)){
#                 studyTable<-data.frame(uid=smp,IMC_text_file=smp,study='NO_STUDY',sample='NO_sample',replicte='NO_replicate',ROI='NO_ROI',bioGroup='NO_BIOGROUP',stringsAsFactors = F)
#               }
#               newDir<-checkDir(filePathName,'rasterStacks')
#               newDir<-checkDir(filePathName,'rasters')
#               newDir<-checkDir(paste(filePathName,'rasters',sep='/'),studyTable$IMC_text_file[studyTable$uid==smp])
#
#
#               rstFile<-lapply(names(x[[smp]]),function(chnl){
#
#                 fileObjective<-paste0(newDir,'/',chnl,'.nc')
#
#                 if (raster::fromDisk(x[[smp]][[chnl]])){
#                   temp_raster<-raster::readAll(x[[smp]][[chnl]])
#                   unlink(fileObjective,force = T)
#                   raster::writeRaster(x = temp_raster,
#                                       filename = fileObjective,
#                                       overwrite=T,
#                                       format='CDF')} else{
#                                         raster::writeRaster(x = x[[smp]][[chnl]],
#                                                             filename = fileObjective,
#                                                             overwrite=T,
#                                                             format='CDF')
#                                       }
#                 return(fileObjective)
#               })
#
#
#               stackDetails<-studyTable[studyTable$uid==smp,]
#               rstrStk<-IMC_stack(x = rstFile,
#                                  uid = smp,
#                                  IMC_text_file = stackDetails$IMC_text_file,
#                                  study = stackDetails$study,
#                                  sample = stackDetails$sample,
#                                  replicate = stackDetails$replicate,
#                                  ROI = stackDetails$ROI,
#                                  bioGroup = stackDetails$bioGroup,
#                                  channels = data.frame(RcolumnNames = names(x[[smp]])))
#
#
#               IMCstackSave(x = rstrStk,
#                            filename = paste0(filePathName,'/rasterStacks/',
#                                              studyTable$IMC_text_file[studyTable$uid==smp],'.stk'))
#
#               return(rstrStk)
#             },USE.NAMES = T,simplify = F)
#
#
#             rstrStk<-new('IMC_RsCollection',rstrStk)
#
#
#             if (objectReturn){
#               return(rstrStk)
#             }
#             return(paste0(filePathName,'/rasterStacks'))
#           })
#
# #*** archive:imc_classification ---------------------------------------------------
#
# setMethod('archive',signature = c('IMC_Classification'),
#           function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,studyTable=NULL,...){
#
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             # if (!endsWith(filePathName,'.txt')) filePathName<-paste0(filePathName,'.txt')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('Classification is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             dir.create(filePathName,showWarnings = F)
#
#             rstrStk<-sapply(names(x),function(smp){
#
#               if (is.null(studyTable)){
#                 studyTable<-data.frame(uid=smp,IMC_text_file=smp,study='NO_STUDY',sample='NO_sample',replicte='NO_replicate',ROI='NO_ROI',bioGroup='NO_BIOGROUP',stringsAsFactors = F)
#               }
#               newDir<-checkDir(filePathName,'rasterStacks')
#               newDir<-checkDir(filePathName,'rasters')
#               newDir<-checkDir(paste(filePathName,'rasters',sep='/'),studyTable$IMC_text_file[studyTable$uid==smp])
#
#
#               rstFile<-lapply(names(x[[smp]]),function(chnl){
#
#                 fileObjective<-paste0(newDir,'/',chnl,'.grd')
#
#                 if (raster::fromDisk(x[[smp]][[chnl]])){
#                   temp_raster<-raster::readAll(x[[smp]][[chnl]])
#                   unlink(fileObjective,force = T)
#                   raster::writeRaster(x = temp_raster,
#                                       filename = fileObjective,
#                                       overwrite=T,
#                                       format='raster')} else{
#                                         raster::writeRaster(x = x[[smp]][[chnl]],
#                                                             filename = fileObjective,
#                                                             overwrite=T,
#                                                             format='raster')
#                                       }
#                 return(fileObjective)
#               })
#
#
#               stackDetails<-studyTable[studyTable$uid==smp,]
#               rstrStk<-IMC_stack(x = rstFile,
#                                  uid = smp,
#                                  IMC_text_file = stackDetails$IMC_text_file,
#                                  study = stackDetails$study,
#                                  sample = stackDetails$sample,
#                                  replicate = stackDetails$replicate,
#                                  ROI = stackDetails$ROI,
#                                  bioGroup = stackDetails$bioGroup,
#                                  channels = data.frame(RcolumnNames = names(x[[smp]])))
#
#
#               IMCstackSave(x = rstrStk,
#                            filename = paste0(filePathName,'/rasterStacks/',
#                                              studyTable$IMC_text_file[studyTable$uid==smp],'.stk'))
#
#               return(rstrStk)
#             },USE.NAMES = T,simplify = F)
#
#
#             rstrStk<-new('IMC_Classification',rstrStk)
#
#
#             if (objectReturn){
#               return(rstrStk)
#             }
#             return(paste0(filePathName,'/rasterStacks'))
#           })
#
# # #*** archive:imc_classification ---------------------------------------------------
# # setMethod('archive',signature = c('IMC_Classification'),
# #           function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,studyTable=NULL,...){
# #
# #
# #             if (is.null(filePathName)) stop(mError('Provide file name'))
# #             if (!forceSave){
# #               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
# #               artnTimeStmp<-attr(x,'artnTimeStmp')
# #               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
# #                 if (mdtnTimeStmp==artnTimeStmp){
# #                   message(mWarning('Classification file is up to date, you might use "forceSave=T"'))
# #                   return(0)
# #                 }
# #               }
# #             }
# #
# #             fileOK<-try(
# #               x<-sapply(names(x),function(smp){
# #                 smpTxtName<-studyTable$IMC_text_file[studyTable$uid==smp]
# #                 filePath<-file.path(filePathName,paste0(smpTxtName,'.grd'))
# #                 raster::writeRaster(x = x[[smp]],
# #                                     filename = filePath,
# #                                     overwrite=T,
# #                                     format='raster')
# #                 raster(filePath)
# #               }))
# #
# #
# #             if (exists('fileOK')){
# #               if (!is.null(fileOK)){
# #                 if (inherits(fileOK,what = 'try-error')){
# #                   message(mError('Classifier file archiviation failed'))
# #                   return(-1)
# #                 }
# #               }
# #             }
# #             if (objectReturn){
# #               return(x)
# #             }
# #             return(filePathName)
# #             #
# #             #             dir.create(filePathName,showWarnings = F)
# #             #
# #             #             rstrStk<-sapply(names(x),function(smp){
# #             #
# #             #               if (is.null(studyTable)){
# #             #                 studyTable<-data.frame(uid=smp,IMC_text_file=smp,study='NO_STUDY',sample='NO_sample',replicte='NO_replicate',ROI='NO_ROI',bioGroup='NO_BIOGROUP',stringsAsFactors = F)
# #             #               }
# #             #               newDir<-checkDir(filePathName,'rasterStacks')
# #             #               newDir<-checkDir(filePathName,'rasters')
# #             #               newDir<-checkDir(paste(filePathName,'rasters',sep='/'),studyTable$IMC_text_file[studyTable$uid==smp])
# #             #               rstFile<-lapply(names(x[[smp]]),function(chnl){
# #             #
# #             #                 fileObjective<-paste0(newDir,'/',chnl,'.nc')
# #             #                 raster::writeRaster(x = x[[smp]][[chnl]],
# #             #                                     filename = fileObjective,
# #             #                                     overwrite=T,
# #             #                                     format='CDF')
# #             #                 return(fileObjective)
# #             #               })
# #             #
# #             #
# #             #               stackDetails<-studyTable[studyTable$uid==smp,]
# #             #               rstrStk<-IMC_stack(x = rstFile,
# #             #                                  uid = smp,
# #             #                                  IMC_text_file = stackDetails$IMC_text_file,
# #             #                                  study = stackDetails$study,
# #             #                                  sample = stackDetails$sample,
# #             #                                  replicate = stackDetails$replicate,
# #             #                                  ROI = stackDetails$ROI,
# #             #                                  bioGroup = stackDetails$bioGroup,
# #             #                                  channels = data.frame(RcolumnNames = names(x[[smp]])))
# #             #
# #             #
# #             #               IMCstackSave(x = rstrStk,
# #             #                            filename = paste(filePathName,'rasterStacks',
# #             #                                             studyTable$IMC_text_file[studyTable$uid==smp],sep='/'))
# #             #
# #             #               return(rstrStk)
# #             #             },USE.NAMES = T,simplify = F)
# #             #
# #             #
# #             #             if (objectReturn){
# #             #               return(rstrStk)
# #             #             }
# #             #             return(filePathName)
# #           })
# #
#
#
#
# #*** archive:imc_filterframe ---------------------------------------------------
#
# setMethod('archive',signature = 'IMC_FilterFrame',
#           function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,...){
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             if (!endsWith(filePathName,'.IMC_FilterFrame')) filePathName<-paste0(filePathName,'.IMC_FilterFrame')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('Filter definition file is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             fileOK<-try(saveRDS(x,filePathName))
#
#             if (exists('fileOK')){
#               if (!is.null(fileOK)){
#                 if (inherits(fileOK,what = 'try-error')){
#                   message(mError('Filter definition file archiviation failed'))
#                   return(-1)
#                 }
#               }
#             }
#             if (objectReturn){
#               return(x)
#             }
#             return(filePathName)
#           })
#
#
# #*** archive:imc_extractiondirectives ---------------------------------------------------
#
# setMethod('archive',signature = c('IMC_ExtractionDirectives'),
#           function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){
#
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             if (!endsWith(filePathName,'.IMC_ExtractionDirectives')) filePathName<-paste0(filePathName,'.IMC_ExtractionDirectives')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('Extraction directives file is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             fileOK<-try(saveRDS(x,filePathName))
#
#             if (exists('fileOK')){
#               if (!is.null(fileOK)){
#                 if (inherits(fileOK,what = 'try-error')){
#                   message(mError('Extraction directives file archiviation failed'))
#                   return(-1)
#                 }
#               }
#             }
#             if (objectReturn){
#               return(x)
#             }
#             return(filePathName)
#           })
#
# #*** archive:imc_classificationdirectives ---------------------------------------------------
#
# setMethod('archive',signature = c('IMC_ClassificationDirectives'),
#           function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){
#
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             if (!endsWith(filePathName,'.IMC_ClassificationDirectives')) filePathName<-paste0(filePathName,'.IMC_ClassificationDirectives')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('Classification directives file is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             fileOK<-try(saveRDS(x,filePathName))
#
#             if (exists('fileOK')){
#               if (!is.null(fileOK)){
#                 if (inherits(fileOK,what = 'try-error')){
#                   message(mError('Classification directives file archiviation failed'))
#                   return(-1)
#                 }
#               }
#             }
#             if (objectReturn){
#               return(x)
#             }
#             return(filePathName)
#           })
#
# #*** archive:imc_segmantationdirectives ---------------------------------------------------
#
# setMethod('archive',signature = c('IMC_SegmentationDirectives'),
#           function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){
#
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             if (!endsWith(filePathName,'.IMC_SegmentationDirectives')) filePathName<-paste0(filePathName,'.IMC_SegmentationDirectives')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('Segmentation directives file is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             fileOK<-try(saveRDS(x,filePathName))
#
#             if (exists('fileOK')){
#               if (!is.null(fileOK)){
#                 if (inherits(fileOK,what = 'try-error')){
#                   message(mError('Segmentation directives file archiviation failed'))
#                   return(-1)
#                 }
#               }
#             }
#             if (objectReturn){
#               return(x)
#             }
#             return(filePathName)
#           })
#
# #*** archive:imc_InterpretationMatrix ---------------------------------------------------
#
# setMethod('archive',signature = c('IMC_InterpretationMatrix'),
#           function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){
#
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             if (!endsWith(filePathName,'.IMC_InterpretationMatrix')) filePathName<-paste0(filePathName,'.IMC_InterpretationMatrix')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('Interpretation matrix file is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             fileOK<-try(saveRDS(x,filePathName))
#
#             if (exists('fileOK')){
#               if (!is.null(fileOK)){
#                 if (inherits(fileOK,what = 'try-error')){
#                   message(mError('Interpretation matrix file archiviation failed'))
#                   return(-1)
#                 }
#               }
#             }
#             if (objectReturn){
#               return(x)
#             }
#             return(filePathName)
#           })
#
# #*** archive:imc_trainingfeatures ---------------------------------------------------
#
# setMethod('archive',signature = c('IMC_TrainingFeatures'),
#           function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,...){
#
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             if (!endsWith(filePathName,'IMC_TrainingFeatures')) filePathName<-paste0(filePathName,'.IMC_TrainingFeatures')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('Training features file is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             dir.create(filePathName,showWarnings = F)
#             fileOK<-try(utils::write.table(x$value,paste(filePathName,'value.txt',sep='/'),row.names = F,quote = F,sep = '\t'))
#
#             if (exists('fileOK')){
#               if (!is.null(fileOK)){
#                 if (inherits(fileOK,what = 'try-error')){
#                   message(mError('Training features VALUE file archiviation failed'))
#                   return(-1)
#                 }
#               }
#             }
#
#             fileOK<-try(utils::write.table(x$geometry,paste(filePathName,'geometry.txt',sep='/'),row.names = F,quote = F,sep = '\t'))
#
#             if (exists('fileOK')){
#               if (!is.null(fileOK)){
#                 if (inherits(fileOK,what = 'try-error')){
#                   message(mError('Training features GEOMETRY file archiviation failed'))
#                   return(-1)
#                 }
#               }
#             }
#
#
#             if (objectReturn){
#               return(x)
#             }
#             return(filePathName)
#           })
#
#
# #*** archive:imc_segmentationList ---------------------------------------------------
#
# setMethod('archive',signature = c('IMC_SegmentationList'),
#           function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){
#
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             if (!endsWith(filePathName,'.IMC_SegmentationList')) filePathName<-paste0(filePathName,'.IMC_SegmentationList')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('Segmentation file is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             fileOK<-try(saveRDS(x,filePathName))
#
#             if (exists('fileOK')){
#               if (!is.null(fileOK)){
#                 if (inherits(fileOK,what = 'try-error')){
#                   message(mError('Segmentation file archiviation failed'))
#                   return(-1)
#                 }
#               }
#             }
#             if (objectReturn){
#               return(x)
#             }
#             return(filePathName)
#           })
#
#
# #*** archive:imc_expressionmatrix ---------------------------------------------------
#
# setMethod('archive',signature = c('sf','missing'),
#           function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,...){
#
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             if (!endsWith(filePathName,'.sqlite')) filePathName<-paste0(filePathName,'.sqlite')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('Expression file is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             fileOK<-try( sf::st_write(x,filePathName,append=F,quiet=T))
#
#             if (exists('fileOK')){
#               if (!is.null(fileOK)){
#                 if (inherits(fileOK,what = 'try-error')){
#                   message(mError('Expression file archiviation failed'))
#                   return(-1)
#                 }
#               }
#             }
#             if (objectReturn){
#               return(x)
#             }
#             return(filePathName)
#           })
#
# #*** archive:imc_classifier ---------------------------------------------------
#
# setMethod('archive',signature = c('IMC_Classifier'),
#           function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){
#
#
#             if (is.null(filePathName)) stop(mError('Provide file name'))
#             if (!endsWith(filePathName,'.IMC_Classifier')) filePathName<-paste0(filePathName,'.IMC_Classifier')
#             if (!forceSave){
#               mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
#               artnTimeStmp<-attr(x,'artnTimeStmp')
#               if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
#                 if (mdtnTimeStmp==artnTimeStmp){
#                   message(mWarning('Classifier file is up to date, you might use "forceSave=T"'))
#                   return(0)
#                 }
#               }
#             }
#
#             fileOK<-try(saveRDS(x,filePathName))
#
#             if (exists('fileOK')){
#               if (!is.null(fileOK)){
#                 if (inherits(fileOK,what = 'try-error')){
#                   message(mError('Classifier file archiviation failed'))
#                   return(-1)
#                 }
#               }
#             }
#             if (objectReturn){
#               return(x)
#             }
#             return(filePathName)
#           })
#
# #*** archive:NULL---------------------------------------------------
#
# setMethod('archive',signature = c('NULL'),
#           function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){
#
#             object<-deparse(substitute(x))
#             message(mWarning(paste0(object,' is NULL, nothing to archive')))
#             return(-1)
#
#           })


#** retrieve ---------------------------------------------------
# if (!isGeneric("retrieve")) {
#   setGeneric("retrieve", function(...)
#     standardGeneric("retrieve"))
# }
#
#
# setMethod('retrieve',signature = ('environment'),
#           function(x=NULL,
#                    fn_what=NULL,
#                    fn_file=NULL,...){
#
#
#           })
#
#
# setMethod('retrieve',signature = ('character'),
#           function(fn_file=NULL,...){
#             retrieve.generic(fn_file)
#           })
#
#


#* other_methods ---------------------------------------------------
#**  IMC_stack---------------------------------------------------

if (!isGeneric("IMC_stack")) {
  setGeneric("IMC_stack", function(x, ...)
    standardGeneric("IMC_stack"))
}


setMethod('IMC_stack',signature(x='list'),
          function(x,
                   uid,
                   IMC_text_file,
                   study,
                   sample,
                   replicate,
                   ROI,
                   bioGroup,
                   channels,
                   ...){

            rstrStk<-raster::stack(x)
            rstrStk<-methods::as(rstrStk,'IMC_RasterStack')
            if (!missing(uid)) {rstrStk@uid<-uid} else {stop('uid missing')}
            if (!missing(IMC_text_file)) {rstrStk@IMC_text_file<-IMC_text_file} else {stop('IMC original file missing')}
            if (!missing(study)) {
              if (!is.null(study)) {rstrStk@study<-study} else {rstrStk@study<-'NO_study'}}
            if (!missing(sample)) {
              if (!is.null(sample)) {rstrStk@sample<-sample} else {rstrStk@sample<-'NO_sample'}}
            if (!missing(ROI)) {
              if (!is.null(ROI)) {rstrStk@ROI<-ROI} else {rstrStk@ROI<-'NO_ROI'}}
            if (!missing(replicate)) {
              if (!is.null(replicate)) {rstrStk@replicate<-replicate} else {rstrStk@replicate<-'NO_replicate'}}
            if (!missing(bioGroup)) {
              if (!is.null(bioGroup)) {rstrStk@bioGroup<-bioGroup} else {rstrStk@bioGroup='NO_bioGroup'}}
            if (!missing(channels)) {rstrStk@channels<-channels}
            return(rstrStk)
          })

setMethod('IMC_stack',signature(x='IMC_RasterStack'),
          function(x,...){
            rasterStackList<-list(x,...)
            uid<-Reduce('=',lapply(rasterStackList,slot,'uid'))
            IMC_text_file<-Reduce('=',lapply(rasterStackList,slot,'IMC_text_file'))
            study<-Reduce('=',lapply(rasterStackList,slot,'study'))
            sample<-Reduce('=',lapply(rasterStackList,slot,'sample'))
            replicate<-Reduce('=',lapply(rasterStackList,slot,'replicate'))
            ROI<-Reduce('=',lapply(rasterStackList,slot,'ROI'))
            bioGroup<-Reduce('=',lapply(rasterStackList,slot,'bioGroup'))
            channels<-Reduce('=',lapply(rasterStackList,slot,'channels'))
            newStack<-raster::stack(rasterStackList)
            newStack<-as(newStack,'IMC_RasterStack')
            newStack@uid<-uid
            newStack@IMC_text_file<-IMC_text_file
            newStack@study<-study
            newStack@sample<-sample
            newStack@replicate<-replicate
            newStack@ROI<-ROI
            newStack@bioGroup<-bioGroup
            newStack@channels<-channels
            return(newStack)
          })


#**  cleanup classification---------------------------------------------------
if (!isGeneric("cleanUpClassification")) {
  setGeneric("cleanUpClassification", function(x,
                                               Npixels=2,
                                               directioMethod=4,
                                               layerLabel='label',
                                               newLayerLabel='cleanUP',
                                               label=NULL,
                                               dumpLabel='undetermined',...)
    standardGeneric("cleanUpClassification"))
}

#' @export
setMethod('cleanUpClassification',signature(x='IMC_Classification'),
          function(x,
                   Npixels=2,
                   directioMethod=4,
                   layerLabel='label',
                   newLayerLabel='cleanUP',
                   label=NULL,
                   dumpLabel='undetermined',...){

            newClassification<-cleanUpAntiClumps(x,
                                                 fn_Ncount = Npixels,
                                                 fn_directions = directioMethod,
                                                 fn_layerLabel = layerLabel,
                                                 fn_newLayerLabel = newLayerLabel,
                                                 fn_label = label,
                                                 fn_dumpLabel = dumpLabel)

            newClassification<-new('IMC_Classification',newClassification)
            return(newClassification)

          }
)

#' @export
setMethod('cleanUpClassification',signature(x='environment'),
          function(x,
                   Npixels=2,
                   directioMethod=4,
                   layerLabel='label',
                   newLayerLabel='cleanUP',
                   label=NULL,
                   dumpLabel='undetermined',...){

            if (is.null(x$currentAnalysis$classification)) stop(mError('coud not find any classification'))

            oldClassification<-x$currentAnalysis$classification
            newClassification<-cleanUpAntiClumps(oldClassification,
                                                 fn_Ncount = Npixels,
                                                 fn_directions = directioMethod,
                                                 fn_layerLabel = layerLabel,
                                                 fn_newLayerLabel=newLayerLabel,
                                                 fn_label = label,
                                                 fn_dumpLabel = dumpLabel)

            newClassification<-new('IMC_Classification',newClassification)


            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

            attr(newClassification,'crtnTimeStmp')<-attr(x$currentAnalysis$classification,'crtnTimeStmp')
            attr(newClassification,'mdtnTimeStmp')<-newTimeStmp

            if (!is.null(attr(x$currentAnalysis$classification,'fileArchive'))){
              attr(newClassification,'artnTimeStmp')<-newTimeStmp
              attr(newClassification,'fileArchive')<-attr(x$currentAnalysis$classification,'fileArchive')
            }

            x$currentAnalysis$classification<-newClassification

            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

          }
)

#**  concentric classification---------------------------------------------------
if (!isGeneric("concentricClassification")) {
  setGeneric("concentricClassification", function(x,
                                                  classificationLyr=NULL,
                                                  label=NULL,
                                                  type = NULL,
                                                  area=NULL,
                                                  prefix='topoMap_',
                                                  raster=NULL,
                                                  derivedRaster=NULL,
                                                  features=NULL,
                                                  maxClumps=10,
                                                  clumpDirection=8,
                                                  mtry=length(features)/3,
                                                  ntree=100,
                                                  trace=10,...)
             standardGeneric("concentricClassification"))
}

#' @export
setMethod('concentricClassification',signature(x='IMC_Classification'),
          function(x,
                   classificationLyr=NULL,
                   label=NULL,
                   type = NULL,
                   area=NULL,
                   prefix='topoMap_',
                   raster=NULL,
                   derivedRaster=NULL,
                   features=NULL,
                   maxClumps=10,
                   clumpDirection=8,
                   mtry=length(features)/3,
                   ntree=100,
                   trace=10,...){

            newClassification<-topoMap(fn_rstStack=x,
                                       fn_layerLabel=classificationLyr,
                                       fn_label=label,
                                       fn_area=area,
                                       fn_prefix=prefix,
                                       fn_raster=raster,
                                       fn_derivedRaster=derivedRaster,
                                       fn_features=features,
                                       fn_maxClumps=maxClumps,
                                       fn_clumpDirection=clumpDirection,
                                       fn_mtry=mtry,
                                       fn_ntree=ntree,
                                       fn_trace=trace)

            newClassification<-new('IMC_Classification',newClassification)
            return(newClassification)

          }
)


#' @export
setMethod('concentricClassification',signature(x='environment'),
          function(x,
                   classificationLyr=NULL,
                   label=NULL,
                   type = NULL,
                   area=NULL,
                   prefix='topoMap_',
                   raster=NULL,
                   derivedRaster=NULL,
                   features=NULL,
                   maxClumps=10,
                   clumpDirection=8,
                   mtry=length(features)/3,
                   ntree=100,
                   trace=10,...){

            if (is.null(x$currentAnalysis$classification)) stop(mError('coud not find any classification'))
            if (is.null(label)) label<-tf_labelList(x)
            if (is.null(area)) area<-tf_areaQuantile(x,c(0,1))
            if (is.null(raster)){
              if (!is.null(x$raster)) raster<-x$raster
            }
            if (is.null(derivedRaster)){
              if (!is.null(x$currentAnalysis$derivedRasters)) derivedRaster<-x$currentAnalysis$derivedRasters
            }
            if (is.null(features)){
              if (!is.null(x$currentAnalysis$classificationDirectives@methodParameters$predictiveFeatures)){
                features<-x$currentAnalysis$classificationDirectives@methodParameters$predictiveFeatures
              } else {stop(mError('cannot find features names'))}}
            if (is.null(type)){
              stop(mErro('specify type, either "training" or "test"'))
            }

            switch(type,
                   test={
                     newClassification<-topoMap(fn_rstStack=x$currentAnalysis$classification,
                                                fn_layerLabel=classificationLyr,
                                                fn_label=label,
                                                fn_area=area,
                                                fn_prefix=prefix,
                                                fn_raster=raster,
                                                fn_derivedRaster=derivedRaster,
                                                fn_features=features,
                                                fn_maxClumps=maxClumps,
                                                fn_clumpDirection=clumpDirection,
                                                fn_mtry=mtry,
                                                fn_ntree=ntree,
                                                fn_trace=trace)
                   },
                   training = {
                     newClassification<-topoMap_tf(fn_rstStack=x$currentAnalysis$classification,
                                                   fn_layerLabel=classificationLyr,
                                                   fn_label=label,
                                                   fn_prefix=prefix,
                                                   fn_raster=raster,
                                                   fn_derivedRaster=derivedRaster,
                                                   fn_trainingFeatures = x$currentAnalysis$trainingFeatures,
                                                   fn_features=features,
                                                   fn_mtry=mtry,
                                                   fn_ntree=ntree,
                                                   fn_trace=trace)
                   }
            )

            newClassification<-new('IMC_Classification',newClassification)


            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())

            attr(newClassification,'crtnTimeStmp')<-attr(x$currentAnalysis$classification,'crtnTimeStmp')
            attr(newClassification,'mdtnTimeStmp')<-newTimeStmp

            if (!is.null(attr(x$currentAnalysis$classification,'fileArchive'))){
              attr(newClassification,'artnTimeStmp')<-newTimeStmp
              attr(newClassification,'fileArchive')<-attr(x$currentAnalysis$classification,'fileArchive')
            }

            x$currentAnalysis$classification<-newClassification

            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

          }
)
