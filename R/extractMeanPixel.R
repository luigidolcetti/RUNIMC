#'Some BullShit
#'
#'
#' @export
extractMeanPixel<-function(fn_polygons,
                           fn_raster){


  if (length(unique(fn_polygons$uid))!=0){
    value_x<-lapply(unique(fn_polygons$uid),function(x){

      sub_raster<-fn_raster[[x]]
      sub_condensedPolygon<-fn_polygons[fn_polygons$uid==x,]
      value<-exactextractr::exact_extract(sub_raster,sub_condensedPolygon,'mean')
      colnames(value)<-names(sub_raster)
      value<-dplyr::bind_cols(sub_condensedPolygon,value)

    })
    value_x<-do.call(rbind.data.frame,value_x)
    return(value_x)
  }
}
