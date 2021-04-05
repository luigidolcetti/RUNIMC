if (!isGeneric("localCorrection")) {
  setGeneric("localCorrection", function(x,
                                         labelLayer='label',
                                         suffix = '_clean',
                                         matrixExtent = 3,
                                         paddingLabel = 'undetermined',
                                         ...)
    standardGeneric("localCorrection"))
}

#' Clean up Classification
#'
#' Method used to 'clean' the classification layer. Essentially a wrapper around
#'   [raster::focal()] that use a function that substitute the classification of
#'   a pixel with the most abundant one in the square matrix defined by the parameter
#'   matrixExtent.
#'
#' @param x environment, the study whom classification is to modify
#' @param labelLayer character, the name of the layer to modify. Must be a discrete
#'   value layer.
#' @param matrixExtent numeric, extent of the square matrix to use in the focal
#'   function, must be an odd number.
#' @param suffix character, suffix to add to labelLayer to form the new classification
#'   layer name.
#' @param paddingLabel character, a class to be used for padding. Must be a class
#'   already present in the classification to modify.
#' @return an IMC_classification object that will overwrite the current analysis
#'   classification
#' @export
setMethod('localCorrection',signature = ('environment'),
          function(x,
                   labelLayer='label',
                   suffix = '_clean',
                   matrixExtent = 3,
                   paddingLabel = 'undetermined',
                   ...){


            chkLayer<-lapply(x$currentAnalysis$classification,names)

            chkLayer<-sapply(chkLayer,function(x){any(labelLayer %in% x)},simplify = T,USE.NAMES = F)

            if (!all(chkLayer)) stop(RUNIMC:::mError('cannot find specified labelLayer'),call. = F)

            chkRAT<-lapply(x$currentAnalysis$classification,function(x){
              raster::levels(x[[labelLayer]])[1]
            })

            chkRATnull<-sapply(chkRAT,is.null,simplify = T,USE.NAMES = F)

            if (any(chkRATnull)) stop(RUNIMC:::mError('not all classification have classes'),call. = F)

            chkRATpadding<-sapply(chkRAT,function(x){
              paddingLabel %in% x[[1]][,2]
            },simplify = T,USE.NAMES = F)

            if (!all(chkRATpadding)) stop(RUNIMC:::mError('some class table do not contain paddingLabel'),call.=F)

            pdv<-unique(sapply(chkRAT,function(x){
              x[[1]][x[[1]][,2]==paddingLabel,1]
            },simplify = T,USE.NAMES = F))

            if (length(pdv)>1) stop(RUNIMC:::mError('problems with padding value matching more than one ID'),call. = F)

            if ((matrixExtent %% 2)==0) stop(RUNIMC:::mError('matrixExtent must be a odd number'))


            uids<-st_uids(x)

            oldStk<-as.list(x$currentAnalysis$classification)

            newStk<-sapply(uids,function(i){
              cat (paste0('cleaning up ',labelLayer,' of ',i,'\n'))
              rst<-newStk[[i]][[labelLayer]]
              filePath<-raster::filename(rst)
              fileN<-fs::path_file(filePath)
              fileD<-fs::path_dir(filePath)
              fileE<-sub('.*\\.', '',fileN)
              fileNN<-sub('\\.[^.]*$','',fileN)
              newRst<-raster::focal(x = rst,
                                    w = matrix(1, ncol=matrixExtent,nrow = matrixExtent),
                                    fun = function(x){
                                      tableX<-table(x)
                                      wM<-which.max(tableX)[1]
                                      nV<-as.numeric(names(tableX)[wM])
                                      return(nV)
                                    },
                                    filename = file.path(fileD,
                                                         paste0(fileNN,suffix,'.',fileE)),
                                    pad=T,
                                    padValue=pdv,
                                    overwrite = T,
                                    progress='text')
              newRst<-raster::ratify(newRst)
              levels(newRst)<-raster::levels(rst)
              newName<-paste0(labelLayer,suffix)
              names(newRst)<-newName
              newStk[[i]][[newName]]<-newRst

              rstrStk<-IMC_stack(x = newStk,
                                 uid = rst@uid,
                                 IMC_text_file = rst@IMC_text_file,
                                 study = rst@study,
                                 sample = rst@sample,
                                 replicate = rst@replicate,
                                 ROI = rst@ROI,
                                 bioGroup = rst@bioGroup,
                                 channels = rst@channels)

              rstrStk<-IMCstackSave(rstrStk,file.path(fn_filePath,'rasterStacks',paste0(fn_rst@IMC_text_file,'.stk')))
            })



            newClassification<-new('IMC_Classification',newStk)

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


          })
