#'Some BullShit
#'
#'
#' @export
xyFormat<-function(fn_raster){

  if (is.null(fn_raster)) stop(RUNIMC:::mError("Specify a raster"))
    DFraster<-raster::rasterToPoints(fn_raster)
    DFraster<-as.data.frame(DFraster)
    lvls<-raster::levels(fn_raster)[[1]]
    if (!is.null(lvls)) {
      names(DFraster)<-c('x','y','label')
      plotBrakes<-lvls[,1]
      plotLabels<-as.character(lvls[,2])
      scrmbLabels<-strsplit(plotLabels,"_")
      scrmbLabels<-unlist(lapply(scrmbLabels,function(x){
        paste0(x[2],' (',x[1],')')
      }))
      DFraster$label<-factor(x = DFraster$label,
                         levels =plotBrakes,
                         labels = scrmbLabels)}
    return(DFraster)
}

#'Some BullShit
#'
#'
#' @export
xyFormatList<-function(fn_RsCollection){
  if (is.null(fn_RsCollection)) stop(RUNIMC:::mError("Specify a raster collection"))

  out<-sapply(names(fn_RsCollection),function(nms){

    DFraster<-xyFormat(fn_RsCollection[[nms]])
    DFraster<-as.data.frame(DFraster)
    # DFraster<-cbind.data.frame(DFraster,data.frame(uid=rep(as.character(nms),nrow(DFraster))))
    DFraster<-data.frame(uid=as.character(nms),DFraster,row.names = NULL)

  },USE.NAMES = T,simplify = F)

  out<-do.call(rbind.data.frame,list(out,row.names=NULL))

  return(out)
}
