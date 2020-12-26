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

# #** initialize.Classifier ---------------------------------------------------
# setMethod('initialize','IMC_Classifier',
#           function(.Object, ...) {
#             Object <- callNextMethod(.Object, ...)
#
#             Object<-initObjectAttr(Object)
#             return(Object)})

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
#** newAnalysis ---------------------------------------------------
if (!isGeneric("newAnalysis")) {
  setGeneric("newAnalysis", function(x,analysisName=NULL,...)
    standardGeneric("newAnalysis"))
}

#' @export
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
if (!isGeneric("dismissAnalysis")) {
  setGeneric("dismissAnalysis", function(x,analysis=NULL,...)
    standardGeneric("dismissAnalysis"))
}

#' @export
setMethod('dismissAnalysis',signature = ('environment'),
          function(x,analysis=NULL,...){
            browser()
            dsmsFolder<-checkDir(file.path(x$rootFolder,x$name,'analysis'),'dismissed')
            if (is.null(x$analysis)) stop(mError('There are no analysis to dismiss'))
            if (analysis==x$currentAnalysis$name) analysis<-NULL
            if (is.null(analysis)) {
              message(mWarning('No analysis name specified, current analysis will be dismissed, continue?'))
              answr<-readline()
              if (!grepl("y",answr,ignore.case = T)) stop(mWarning('Dismissal aborted'))
              analysis<-x$currentAnalysis$name
              x$analysis<-x$analysis[x$analysis!=analysis]
              file.copy(from = x$currentAnalysis$folder,to = dsmsFolder,overwrite = T,recursive = T)
              unlink(x$currentAnalysis$folder,recursive = T,force = T)
              x$currentAnalysis<-NULL
              message(mMessage(paste0(analysis, ' dismissed')))
              return(0)
            }
            lstAnl<-listAnalysis(x)
            if (!any(analysis %in% lstAnl)) stop(mError("Specified analysis doesn't exist in this study"))
            file.copy(from = file.path(x$rootFolder,x$name,'analysis',analysis),to = dsmsFolder,overwrite = T,recursive = T)
            unlink(file.path(x$rootFolder,x$name,'analysis',analysis),recursive = T,force = T)
            x$analysis<-x$analysis[x$analysis!=analysis]
            return(mMessage(paste0(analysis, ' dismissed')))
          })

#' @export
setMethod('dismissAnalysis',signature = c('missing'),
          function(x,analysis=NULL,...){
            stop(mError('Specify study'))
          })

#** listAnalisys ---------------------------------------------------
if (!isGeneric("listAnalysis")) {
  setGeneric("listAnalysis", function(x,...)
    standardGeneric("listAnalysis"))
}

#' @export
setMethod('listAnalysis',signature = ('environment'),
          function(x,...){
            x$analysis
          })

#** showCurrentAnalysis ---------------------------------------------------

if (!isGeneric("showCurrentAnalysis")) {
  setGeneric("showCurrentAnalysis", function(x,...)
    standardGeneric("showCurrentAnalysis"))
}

#' @export
setMethod('showCurrentAnalysis',signature = ('environment'),
          function(x,...){
            x$currentAnalysis
          })

#** channels ---------------------------------------------------
if (!isGeneric("channels")) {
  setGeneric("channels", function(x, ...)
    standardGeneric("channels"))
}

#' @export
setMethod('channels',signature = ('ANY'),
          function(x,...){
            x$channels
          })

#** rasterFromMarker ---------------------------------------------------
if (!isGeneric("rasterFromMarker")) {
  setGeneric("rasterFromMarker", function(x, marker,...)
    standardGeneric("rasterFromMarker"))
}

#' @export
setMethod('rasterFromMarker',signature = ('ANY'),
          function(x, marker,...){
            chnls<-x@channels$RcolumnNames[grep(marker,
                                                x@channels$marker,
                                                ignore.case = T)]
            names(chnls)<-x@channels$marker[grep(marker,
                                                 x@channels$marker,
                                                 ignore.case = T)]
            return(chnls)
          })

#** channelsAll ---------------------------------------------------
if (!isGeneric("channelsAll")) {
  setGeneric("channelsAll", function(x, ...)
    standardGeneric("channelsAll"))
}

#' @export
setMethod('channelsAll',signature = ('ANY'),
          function(x,...){
            x$channels$RcolumnNames[x$channels$loaded]
          })

#** addFilter ---------------------------------------------------
if (!isGeneric("addFilter")) {
  setGeneric("addFilter", function(x,filter=NULL,parameters=NULL,channels=NULL,append=T,...)
    standardGeneric("addFilter"))
}

#' @export
setMethod('addFilter',signature = ('environment'),
          function(x,filter=NULL,parameters=NULL,channels=NULL,append=T,...){

            if (is.null(filter)) stop(mError('provide Filter'))
            if (is.null(parameters)) stop(mError(paste0('provide parameters for ',filter,' filter')))
            if (is.null(channels)) stop(mError(paste0('provide channels to elaborate with ',filter,' filter')))
            if (append) x$currentAnalysis$filters<-x$currentAnalysis$filters else x$currentAnalysis$filters<-NULL
            if (is.null(x$currentAnalysis$filters)) {
              newFilter<-new('IMC_FilterFrame')
              x$currentAnalysis$filters<-newFilter
            }

            newFilter<-data.frame(filter=filter,
                                  parameters=I(list(parameters)),
                                  channels=I(list(channels)))


            oldFilters<-x$currentAnalysis$filters
            newFilter<-rbind.data.frame(oldFilters,newFilter)
            newFilter<-new('IMC_FilterFrame',newFilter)

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
            x$currentAnalysis$filters<-newFilter

          })

#** deployFilters ---------------------------------------------------
if (!isGeneric("deployFilters")) {
  setGeneric("deployFilters", function(x,saveToDisk=T, ...)
    standardGeneric("deployFilters"))
}

#' @export
setMethod('deployFilters',signature = ('environment'),
          function(x,saveToDisk=T...){
browser()
            if (saveToDisk){
              oldRstk<-list.files(file.path(x$currentAnalysis$folder,'rasterStacks'),full.names = T,recursive = F)
              unlink(oldRstk,recursive = T)
              oldRst<-list.dirs(file.path(x$currentAnalysis$folder,'rasters'),full.names = T,recursive = F)
              unlink(oldRst,recursive = T)
              message(mMessage('Clean up raster and rasterStack folders'))
            }

            ff<-x$currentAnalysis$filters
            derivedRasters<-apply(ff,1,function(fltDf){
              imageRFilter(fn_rasterStack = x$raster,
                           fn_filter = fltDf$filter,
                           fn_filterParameterList = fltDf$parameters,
                           fn_markerList = fltDf$channels,
                           fn_saveToDisk = saveToDisk,
                           fn_pathToFile = x$currentAnalysis$folder)
            })


            rstNms<-Reduce('=',lapply(derivedRasters,names))
            derivedRasters<-unlist(derivedRasters,recursive=F)
            derivedRasters<-sapply(rstNms,function(x){
              rasterStackList<-derivedRasters[rstNms==x]


              uid<-Reduce('=',lapply(rasterStackList,slot,'uid'))
              IMC_text_file<-Reduce('=',lapply(rasterStackList,slot,'IMC_text_file'))
              study<-Reduce('=',lapply(rasterStackList,slot,'study'))
              sample<-Reduce('=',lapply(rasterStackList,slot,'sample'))
              replicate<-Reduce('=',lapply(rasterStackList,slot,'replicate'))
              ROI<-Reduce('=',lapply(rasterStackList,slot,'ROI'))
              bioGroup<-Reduce('=',lapply(rasterStackList,slot,'bioGroup'))
              channels<-Reduce('=',lapply(rasterStackList,slot,'channels'))
              IMC_stack(rasterStackList,
                        uid,
                        IMC_text_file,
                        study,
                        sample,
                        replicate,
                        ROI,
                        bioGroup,
                        channels)},USE.NAMES = T)

            derivedRasters<-new('IMC_RsCollection',derivedRasters)

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

            if (saveToDisk){
              sapply(names(derivedRasters),function(nms){
                IMCstackSave(derivedRasters[[nms]],
                             file.path(x$currentAnalysis$folder,
                                       'rasterStacks',
                                       paste0(derivedRasters[[nms]]@IMC_text_file,'.stk')))
              })
            }

            x$currentAnalysis$derivedRasters<-derivedRasters

          })

#** addExtractionDirectives ---------------------------------------------------
if (!isGeneric("addExtractionDirictives")) {
  setGeneric("addExtractionDirictives", function(x,coverage=NULL,prefix=NULL,append=T,...)
    standardGeneric("addExtractionDirictives"))
}

#' @export
setMethod('addExtractionDirictives',signature = ('environment'),
          function(x,coverage=NULL,prefix=NULL,append=T,...){


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
if (!isGeneric("extractTrainingFeatures")) {
  setGeneric("extractTrainingFeatures", function(x,...)
    standardGeneric("extractTrainingFeatures"))
}

#' @export
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


            if (is.null(method)) stop(mError('provide a method'))
            if (is.null(methodParameters)) stop(mError(paste0('provide parameters for ',method) ))


            newDirectives<-new('IMC_ClassificationDirectives',
                               method=method,
                               methodParameters=methodParameters)

            x$currentAnalysis$classificationDirectives<-newDirectives

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
          })

#** makeClassificationModel ---------------------------------------------------
if (!isGeneric("makeClassificationModel")) {
  setGeneric("makeClassificationModel", function(x,seed=1234, ...)
    standardGeneric("makeClassificationModel"))
}

#' @export
setMethod('makeClassificationModel',signature = ('environment'),
          function(x,seed=1234,...){



            if (is.null(x$currentAnalysis$classificationDirectives)) stop(mError('Before making a classification model directives must be specified'))
            switch(x$currentAnalysis$classificationDirectives@method,

                   randomForest = {

                     set.seed=seed
                     rVar<-x$currentAnalysis$classificationDirectives@methodParameters$responseVariable
                     pFtr<-x$currentAnalysis$classificationDirectives@methodParameters$predictiveFeatures
                     cPrm<-x$currentAnalysis$classificationDirectives@methodParameters[!(names(x$currentAnalysis$classificationDirectives@methodParameters) %in%c('responseVariable','predictiveFeatures'))]
                     fFormula<-eval(parse(text=paste0(rVar,'~',paste(pFtr,collapse = '+'))))
                     rFcall<-c(list(formula = fFormula,
                                    data = x$currentAnalysis$trainingFeatures$value),
                               cPrm)
                     rf_classifier <- do.call(randomForest::randomForest,rFcall)

                     rf_classifier<-as(rf_classifier,'IMC_Classifier')
                     rf_classifier<-initObjectAttr(rf_classifier)
                     x$currentAnalysis$classifier<-rf_classifier

                     newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
                     attr(x,'mdtnTimeStmp')<-newTimeStmp
                     attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
                   }
            )
          })

#** classify ---------------------------------------------------
if (!isGeneric("classify")) {
  setGeneric("classify", function(x,...)
    standardGeneric("classify"))
}

#' @export
setMethod('classify',signature = ('environment'),
          function(x,...){

            if (is.null(x$currentAnalysis$classifier)) stop(mError('coud not find a classification model to apply'))
            clfrClass<-paste(class(TEST$currentAnalysis$classifier),collapse = '_')

            switch(clfrClass,
                   randomForest.formula_randomForest = {

                     pFtr<-x$currentAnalysis$classificationDirectives@methodParameters$predictiveFeatures
                     rfCls<-x$currentAnalysis$classifier
                     uids<-x$studyTable$uid

                     TEST_monkey<-sapply(uids,function(uid){

                       rst<-raster::stack(x$raster[[uid]],x$currentAnalysis$derivedRasters[[uid]])
                       mf<-monkeyForest(fn_rst =rst,
                                        fn_layers = pFtr,
                                        fn_newLayerName = 'random_mask',
                                        fn_colN = ncol(rst),
                                        fn_rowN = nrow(rst),
                                        fn_forest =rfCls,
                                        fn_append = F)},USE.NAMES = T)

                   })

            TEST_monkey<-new('IMC_Classification',TEST_monkey)
            x$currentAnalysis$classification<-TEST_monkey

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
          })

#** addInterpretationMatrix ---------------------------------------------------
if (!isGeneric("addInterpretationMatrix")) {
  setGeneric("addInterpretationMatrix", function(x,...)
    standardGeneric("addInterpretationMatrix"))
}

#' @export
setMethod('addInterpretationMatrix',signature = ('environment'),
          function(x,...){

            if (is.null(x$currentAnalysis$classifier)) stop(mError('coud not find a classification model to apply'))

            iMat<-guideMatrix(x$currentAnalysis$classifier)
            for (i in names(iMat)){
              iMat[[i]]$seed[iMat[[i]]$level==i]<-1
              iMat[[i]]$include[iMat[[i]]$level==i]<-1
              iMat[[i]]$action[iMat[[i]]$level!=i]<-2
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


            if (is.null(method)) stop(mError('provide a method'))
            if (is.null(methodParameters)) stop(mError(paste0('provide parameters for ',method) ))


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
  setGeneric("segment", function(x,...)
    standardGeneric("segment"))
}

#' @export
setMethod('segment',signature = ('environment'),
          function(x,...){

            if (is.null(x$currentAnalysis$segmentationDirectives)) stop(mError('Before segmenting directives must be specified'))

            mthd<-x$currentAnalysis$segmentationDirectives@method
            mthdPrmtrs<-x$currentAnalysis$segmentationDirectives@methodParameters
            rstToSegment<-x$currentAnalysis$classification
            iMat<-x$currentAnalysis$interpretationMatrix

            switch(mthd,

                   spiderMap = {

                     polygonsList<-list()
                     for (rst in names(rstToSegment)){
                       for (i in names(iMat)){

                         cat(paste(rst,i,'\n',sep=":::"))
                         interpretationMatrixInstance<-iMat[[i]]
                         group_area<-TEST$currentAnalysis$trainingFeatures$geometry$area[TEST$currentAnalysis$trainingFeatures$geometry$label==i]
                         group_roundness<-TEST$currentAnalysis$trainingFeatures$geometry$roundness[TEST$currentAnalysis$trainingFeatures$geometry$label==i]
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
                                              fn_direction = mthdPrmtrs$direction))
                         polygonsList[[rst]][[i]]<-list()
                         polygonsList[[rst]][[i]]<-TEMP[[1]]


                       }
                     }
                   })


            polygonsList<-new('IMC_SegmentationList',polygonsList)
            x$currentAnalysis$segmentation<-polygonsList

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp
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

#** archive ---------------------------------------------------

if (!isGeneric("archive")) {
  setGeneric("archive", function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,...)
    standardGeneric("archive"))
}

#' @export
setMethod('archive',signature = ('environment'),
          function(x,what=NULL,objectReturn=F,forceSave=F,...){

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
                            forceSave = forceSave)
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
                            filePathName = paste(basePath,'cl',sep='/'),
                            objectReturn = F,
                            forceSave = forceSave)
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
                            forceSave = forceSave)
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
#' @export
setMethod('archive',signature = 'IMC_StudyTable',
          function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,...){

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
#' @export
setMethod('archive',signature = 'IMC_ChannelTable',
          function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,...){

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
#' @export
setMethod('archive',signature = c('IMC_RsCollection'),
          function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,studyTable=NULL,...){


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
#' @export
setMethod('archive',signature = c('IMC_Classification'),
          function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,studyTable=NULL,...){


            if (is.null(filePathName)) stop(mError('Provide file name'))
            if (!endsWith(filePathName,'.IMC_Classification')) filePathName<-paste0(filePathName,'.IMC_Classification')
            if (!forceSave){
              mdtnTimeStmp<-attr(x,'mdtnTimeStmp')
              artnTimeStmp<-attr(x,'artnTimeStmp')
              if (!is.na(mdtnTimeStmp) & !is.na(artnTimeStmp)){
                if (mdtnTimeStmp==artnTimeStmp){
                  message(mWarning('Classification file is up to date, you might use "forceSave=T"'))
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
            #               rstFile<-lapply(names(x[[smp]]),function(chnl){
            #
            #                 fileObjective<-paste0(newDir,'/',chnl,'.nc')
            #                 raster::writeRaster(x = x[[smp]][[chnl]],
            #                                     filename = fileObjective,
            #                                     overwrite=T,
            #                                     format='CDF')
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
            #                            filename = paste(filePathName,'rasterStacks',
            #                                             studyTable$IMC_text_file[studyTable$uid==smp],sep='/'))
            #
            #               return(rstrStk)
            #             },USE.NAMES = T,simplify = F)
            #
            #
            #             if (objectReturn){
            #               return(rstrStk)
            #             }
            #             return(filePathName)
          })




#*** archive:imc_filterframe ---------------------------------------------------
#' @export
setMethod('archive',signature = 'IMC_FilterFrame',
          function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,...){

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
#' @export
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
#' @export
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
#' @export
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
#' @export
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
#' @export
setMethod('archive',signature = c('IMC_TrainingFeatures'),
          function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,...){


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
#' @export
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
#' @export
setMethod('archive',signature = c('sf','missing'),
          function(x,what=NULL,filePathName=NULL,objectReturn=F,forceSave=F,...){


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
#' @export
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
#' @export
setMethod('archive',signature = c('NULL'),
          function(x,filePathName=NULL,objectReturn=F,forceSave=F,...){

            object<-deparse(substitute(x))
            message(mWarning(paste0(object,' is NULL, nothing to archive')))
            return(-1)

          })


#** retrieve ---------------------------------------------------
if (!isGeneric("retrieve")) {
  setGeneric("retrieve", function(x,fn_what=NULL,fn_path=NULL,fn_file=NULL,...)
    standardGeneric("retrieve"))
}

#' @export
setMethod('retrieve',signature = ('environment'),
          function(x=NULL,
                   fn_what=NULL,
                   fn_path=NULL,
                   fn_file=NULL,...){

          })

#' @export
setMethod('retrieve',signature = ('NULL'),
          function(x=NULL,
                   fn_what=NULL,
                   fn_path=NULL,
                   fn_file=NULL,...){


            if (!is.null(fn_path)) searchForRaster<-grepl('rasterStacks',fn_path) else searchForRaster<-F
            # if (is.null(fn_path) & !is.null(fn_file)) stop(RUNIMC:::mError('Please, specify a path where to search for this file'))
            if (is.null(fn_path) & is.null(fn_file)) stop(RUNIMC:::mError('Please, specify a path and file'))
            if (!is.null(fn_path) & !is.null(fn_file)){
              if (!dir.exists(fn_path) & !file.exists(fn_file) & !file.exists(file.path(fn_path,fn_file))) stop(RUNIMC:::mError(paste0('Unable to find: ',fn_path)))}
            if (!is.null(fn_file)) {
              fileExtension<-unlist(strsplit(fn_file,'\\.'))
              if (!length(fileExtension)>1) {stop(RUNIMC:::mError('File without extension, cannot decide were to store this information'))}
              fileExtension<-fileExtension[length(fileExtension)]
              if (!any(fileExtension %in% c('IMC_ChannelTable',
                                            'IMC_StudyTable',
                                            'xml',
                                            'IMC_ClassificationDirectives',
                                            'IMC_Classifier',
                                            'sqlite',
                                            'IMC_FilterFrame',
                                            'IMC_TrainingFeatures',
                                            'IMC_InterpretationMatrix',
                                            'IMC_SegmentationDirectives',
                                            'IMC_SegmentationList',
                                            'IMC_ExtractionDirectives',
                                            'IMC_Classification'))) {stop(RUNIMC:::mError('Unknown file extension'))}
            }
            if (searchForRaster & !exists('fileExtension')){
              objectOut<-RUNIMC:::retrieve.RsCollection(fn_file = fn_path,fn_timeStamp = T)
              return(objectOut)
            }
            if (exists('fileExtension')){
              if (file.exists(fn_file)) {fullPath<-fn_file} else {fullPath<-file.path(fn_path,fn_file) }

              switch(fileExtension,
                     IMC_ChannelTable = {objectOut<-RUNIMC:::retrieve.channelTable(fn_file = fullPath,fn_timeStamp = T); return(objectOut)},
                     IMC_StudyTable = {objectOut<-RUNIMC:::retrieve.studyTable(fn_file = fullPath,fn_timeStamp = T); return(objectOut)},
                     IMC_ClassificationDirectives = {objectOut<-RUNIMC:::retrieve.classificationDirectives(fn_file = fullPath,fn_timeStamp = T); return(objectOut)},
                     IMC_Classifier = {objectOut<-RUNIMC:::retrieve.classifier(fn_file = fullPath,fn_timeStamp = T); return(objectOut)},
                     sqlite = {objectOut<-RUNIMC:::retrieve.expressionMatrix(fn_file = fullPath,fn_timeStamp = T); return(objectOut)},
                     IMC_FilterFrame = {objectOut<-RUNIMC:::retrieve.filterFrame(fn_file = fullPath,fn_timeStamp = T); return(objectOut)},
                     IMC_TrainingFeatures = {objectOut<-RUNIMC:::retrieve.trainingFeatures(fn_file = fullPath,fn_timeStamp = T); return(objectOut)},
                     IMC_InterpretationMatrix = {objectOut<-RUNIMC:::retrieve.interpretationMatrix(fn_file = fullPath,fn_timeStamp = T); return(objectOut)},
                     IMC_SegmentationDirectives = {objectOut<-RUNIMC:::retrieve.segmentationDirectives(fn_file = fullPath,fn_timeStamp = T); return(objectOut)},
                     IMC_SegmentationList = {objectOut<-RUNIMC:::retrieve.segmentationList(fn_file = fullPath,fn_timeStamp = T); return(objectOut)},
                     IMC_ExtractionDirectives = {objectOut<-RUNIMC:::retrieve.extractionDirectives(fn_file = fullPath,fn_timeStamp = T); return(objectOut)},
                     IMC_Classification = {objectOut<-RUNIMC:::retrieve.classification(fn_file = fullPath,fn_timeStamp = T); return(objectOut)},
                     xml = {objectOut<-RUNIMC:::retrieve.xml(fn_file = fullPath,fn_timeStamp = T); return(objectOut)})
            }

          })




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
  setGeneric("cleanUpClassification", function(x,Npixels=2,directioMethod=4, ...)
    standardGeneric("cleanUpClassification"))
}

#' @export
setMethod('cleanUpClassification',signature(x='IMC_Classification'),
          function(x,Npixels=2,directioMethod=4,...){

            newClassification<-cleanUpAntiClumps(x,fn_Ncount = Npixels,fn_directions = directioMethod)

            newClassification<-new('IMC_Classification',newClassification)
            return(newClassification)

          }
)

#' @export
setMethod('cleanUpClassification',signature(x='environment'),
          function(x,Npixels=2,directioMethod=4,...){

            if (is.null(x$currentAnalysis$classification)) stop(mError('coud not find any classification'))
            oldClassification<-x$currentAnalysis$classification
            newClassification<-cleanUpAntiClumps(oldClassification,
                                                 fn_Ncount = Npixels,
                                                 fn_directions = directioMethod)

            newClassification<-new('IMC_Classification',newClassification)
            x$currentAnalysis$classification<-newClassification

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

          }
)
exists('fileExtension')
