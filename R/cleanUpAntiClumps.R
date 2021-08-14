#'Some nice thing
#'
#'
#' @export
cleanUpAntiClumps<-function(fn_rstStack,
                            fn_Ncount=2,
                            fn_directions=4,
                            fn_layerLabel='label',
                            fn_newLayerLabel='cleanUP_label',
                            fn_label=NULL,
                            fn_dumpLabel='undetermined'){

  if (!any(fn_directions %in% c(4,8))) stop(RUNIMC:::mError('directions must be either 4 or 8'))
  if (is.null(fn_label) & dim(fn_Ncount)[1]>1) stop(RUNIMC:::mError('providing a vector of counts require explicit labels'))
  if (!is.null(fn_label) & (length(fn_label)!=dim(fn_Ncount)[1])) stop(RUNIMC:::mError("length of label doesn't match length of count vector"))

  smp<-names(fn_rstStack)

  allLyr<-unlist(lapply(smp,function(x){names(fn_rstStack[[x]])}),recursive = T,use.names = F)

  if (any(allLyr %in% fn_newLayerLabel)) stop(RUNIMC:::mError('new layer name already in use'))


  out_rstStack<-lapply(smp,function(x){


    lyrs<-names(fn_rstStack[[x]])
    if (!any(fn_layerLabel %in% lyrs)) stop(RUNIMC:::mError('cannot find label layer'))

    lvls<-raster::levels(fn_rstStack[[x]][[fn_layerLabel]])[[1]]

    if (!all(fn_label %in% lvls$label)) stop (RUNIMC:::mError('some labels are incorrect'))
    if (!any(fn_dumpLabel %in% lvls$label)) {
      message(RUNIMC:::mMessage(paste0(fn_dumpLabel,' added to labels')))
      lvlsNew<-max(lvls[,1])+1
      lvlsDF<-data.frame(ID=c(lvls$ID,lvlsNew),
                         label=c(as.character(lvls$label),fn_dumpLabel),
                         stringsAsFactors = T)
    } else {
      lvlsDF<-data.frame(ID=lvls$ID,
                         label=as.character(lvls$label),
                         stringsAsFactors = T)
    }

    if (is.null(fn_label)){
      IDtoConsider<-lvls$ID[lvls$label!=fn_dumpLabel]
      Ncount<-rep(fn_Ncount,length(IDtoConsider))       # conditions above guarentee fn_Ncount has 1 elelment
      names(Ncount)<-lvls$ID[lvls$label!=fn_dumpLabel]
    } else {
      IDtoConsider<-lvls$ID[lvls$label %in% fn_label]
      if (length(fn_Ncount)==1){
        Ncount<-rep(fn_Ncount,length(IDtoConsider))
        names(Ncount)<-lvls$ID[lvls$label %in% fn_label]
      } else {
        Ncount<-fn_Ncount       # conditions above guarentee fn_Ncount has right number of rows
        names(Ncount)<-lvls$ID[lvls$label %in% fn_label]
      }
    }

    rstLvels<-raster::deratify(fn_rstStack[[x]][[fn_layerLabel]],att=1,drop=T,complete=T)

    newRst<-lapply(lvls$ID,function(y){

      newRstLvels<-rstLvels
      newRstLvels[newRstLvels!=y]<-0

      if (any(y %in% IDtoConsider)){
        clmps<-raster::clump(newRstLvels,directions=fn_directions,gaps=F)
        clmpsTable<-freq(clmps)
        clmpsOut<-clmpsTable[clmpsTable[,'count']<Ncount[as.character(y)],'value']
        newRstLvels[as.vector(clmps) %in% clmpsOut]<-lvlsDF$ID[lvlsDF$label==fn_dumpLabel]
      }
      return(newRstLvels)
    })
    #
    newRst<-raster::stack(newRst)
    newRst<-raster::calc(newRst,sum)
    attr(newRst,'names')<-attr(fn_rstStack[[x]][[fn_layerLabel]],'names')
    newRst<-raster::ratify(newRst)
    levels(newRst)<-lvlsDF
    if (raster::fromDisk(fn_rstStack[[x]][[fn_layerLabel]])){
      filePath<-raster::filename(fn_rstStack[[x]][[fn_layerLabel]])
      filePath<-sub(pattern = '.grd',replacement = '',x = filePath)
      filePath<-paste0(paste0(filePath,'_',fn_newLayerLabel,'.grd'))
      raster::writeRaster(newRst,filename = filePath,overwrite=T)
      newRst<-raster(filePath)
    }
    newStack<-fn_rstStack[[x]]
    newStack[[fn_newLayerLabel]]<-newRst
    return(newStack)
  })
  names(out_rstStack)<-names(fn_rstStack)
  return(out_rstStack)
}
