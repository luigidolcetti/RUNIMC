#'Some nice thing
#'
#'
#' @export
extractXYPixels<-function(fn_polygons,
                          fn_study,
                          fn_whichSample,
                          fn_whichPopulation,
                          fn_howManyCells,
                          fn_buffer=10,
                          fn_seed=123){

  sub_study<-fn_study[[fn_whichSample]]
  sub_polygons<-fn_polygons[fn_polygons$uid==fn_whichSample,]
  target_polygon<-sub_polygons[sub_polygons$label==fn_whichPopulation,]
  set.seed(fn_seed)
  target_polygon<-target_polygon[sample(1:nrow(target_polygon),fn_howManyCells),]

  partition<-lapply(unique(target_polygon$polygon.id),function(x){
    #
    one_polygon<-target_polygon[target_polygon$polygon.id==x,]
    buffer_polygon<-sf::st_as_sfc(sf::st_bbox(sf::st_buffer(one_polygon,fn_buffer)))
    value<-exactextractr::exact_extract(sub_study,
                         buffer_polygon,
                         include_xy=T,
                         progress=T)
    neightboors<-sf::st_intersects(buffer_polygon,sub_polygons)
    neightboorsWithin<-sf::st_contains(buffer_polygon,sub_polygons[as.vector(unlist(neightboors)),])
    neightboors<-sub_polygons[as.vector(unlist(neightboors)),][as.vector(unlist(neightboorsWithin)),]
    list(raster=value,polygon=neightboors)
  })

  return(partition)

}
