#' Filters list
#'
#' [addFilter()] either initialize or add a new filter to an object of class
#'   IMC_FilterFrame. Filters are then processed via the command [deployFilters()].
#'
#' @param x environment, a study
#' @param filter character, type of filter can be 'vanvliet', 'deriche', 'blur_anisotropic', from the package imageR.
#' @param parameters list, list of named values to be passed to the specific function.
#' @param channels character vector, names of the layers to be processed.
#' @param append logical, FALSE delete the previously added filters, TRUE append the specified filters to the stack of filters already defined.
#'
#' @return an object of class IMC_filterFrame
#'
#' @examples
#' \dontrun{
#' availableLayers<-ch_Rnames(MyStudy)
#' addFilter (x = MyStudy,
#'            filter = 'vanvliet',
#'            parameters = list(list(sigma=1,order=0,axis='x'), list(sigma=1,order=0,axis='y')),
#'            channels = availableLayers,
#'            append = F)
#' addFilter (x = MyStudy,
#'            filter = 'blur_anisotropic',
#'            parameters = list(list(amplitude=9),list(amplitude=3)),
#'            channels = availableLayers,
#'            append=T)
#' deployFilters (MyStudy, saveToDisk = T)
#' }
#'
#' @export
setGeneric("addFilter", function(x,filter=NULL,parameters=NULL,channels=NULL,append=T,...)
  standardGeneric("addFilter"))

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

#' Apply filters
#'
#' [deployFilters()] utilizes the filter definitions stored in the IMC_FilterFrame
#'   object, created via the function [addFilter()].
#'   This function is a wrapper for the internal [imageRFilter()].
#'
#' @param x environment, a study.
#' @param saveToDisk logical, save new rasterStacks to disk? Always advisable.
#'
#' @return a new IMC_RsCollection that will be stored in the object **derivedRasters**.
#'
#' @examples
#' \dontrun{
#' availableLayers<-ch_Rnames(MyStudy)
#' addFilter (x = MyStudy,
#'            filter = 'vanvliet',
#'            parameters = list(list(sigma=1,order=0,axis='x'), list(sigma=1,order=0,axis='y')),
#'            channels = availableLayers,
#'            append = F)
#' addFilter (x = MyStudy,
#'            filter = 'blur_anisotropic',
#'            parameters = list(list(amplitude=9),list(amplitude=3)),
#'            channels = availableLayers,
#'            append=T)
#' deployFilters (MyStudy, saveToDisk = T)
#' }
#'
#' @export
setGeneric("deployFilters", function(x,saveToDisk=T, ...)
  standardGeneric("deployFilters"))

setMethod('deployFilters',signature = ('environment'),
          function(x,saveToDisk=T...){

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

            if (saveToDisk){
              derivedRasters<-sapply(names(derivedRasters),function(nms){
                IMCstackSave(derivedRasters[[nms]],
                             file.path(x$currentAnalysis$folder,
                                       'rasterStacks',
                                       paste0(derivedRasters[[nms]]@IMC_text_file,'.stk')))
              },USE.NAMES = T,simplify = F)
            }

            derivedRasters<-new('IMC_RsCollection',derivedRasters)

            newTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
            attr(x,'mdtnTimeStmp')<-newTimeStmp
            attr(x$currentAnalysis,'mdtnTimeStmp')<-newTimeStmp

            x$currentAnalysis$derivedRasters<-derivedRasters

            if (saveToDisk){
              attr(x$currentAnalysis$derivedRasters,'mdtnTimeStmp')<-newTimeStmp
              attr(x$currentAnalysis$derivedRasters,'artnTimeStmp')<-newTimeStmp
              attr(x$currentAnalysis$derivedRasters,'fileArchive')<-file.path(x$currentAnalysis$folder,'rasterStacks')

            }

          })

