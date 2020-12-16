#'Some BullShit
#'
#'
#' @export
rasterTiles<-function(fn_study,
                      fn_sample,
                      fn_marker){

  DF<-lapply(fn_sample,function(smp){
    DF<-lapply(fn_marker,function(mrkr){
      rst<-fn_study[[fn_sample]][fn_marker]
      cellNumber<-1:length(rst)
      cellxy<-raster::xyFromCell(rst,cellNumber)
      cellValue<-raster::extract(rst,cellNumber)
      cbind.data.frame(sample=smp,
                       marker=mrkr,
                       cell=cellNumber,
                       cellxy,
                       z=cellValue)
    })
    do.call(rbind.data.frame,DF)
  })
  DF<-do.call(rbind.data.frame,DF)
}
