#'Some BullShit
#'
#'
#' @export
extractPolygons<-function(fn_polygonList){

  condensedPoligonList<-
    sapply(names(fn_polygonList),function(sld){

      sapply(names(fn_polygonList[[sld]]),function(mrkr){

        nRowPoly<-length(fn_polygonList[[sld]][[mrkr]]@polygons)

        if (nRowPoly!=0) {
          polygon.id=1:nRowPoly

          geoM<-sf::st_sfc(lapply(fn_polygonList[[sld]][[mrkr]]@polygons,function(x)sf::st_polygon(list(x))))
          geoArea <- sf::st_area(geoM)
          geoPerimeter <-  lwgeom::st_perimeter(geoM)
          geoRoundness<-4*pi*geoArea/(geoPerimeter^2)

          FFrame<-data.frame(uid=rep(sld,nRowPoly),
                             polygon.id=polygon.id,
                             label=rep(mrkr,nRowPoly),
                             color=rep('black',nRowPoly),
                             area=geoArea,
                             perimeter=geoPerimeter,
                             roundness=geoRoundness,
                             stringsAsFactors = F)


          TEMP_poly<-sf::st_sf(FFrame,
                           GEOMETRY=geoM,
                           sf_column_name = "GEOMETRY",
                           stringsAsFactors = F,
                           sfc_last = T,
                           row.names = polygon.id)
        }
      },USE.NAMES=T,simplify = F)},USE.NAMES=T,simplify = F)


  condensedPoligonList<-unlist(condensedPoligonList,recursive = F)
  condensedPoligonList<-condensedPoligonList[!unname(unlist(lapply(condensedPoligonList,is.null)))]
  condensedPoligonList<-do.call(rbind.data.frame,condensedPoligonList)
  rownames(condensedPoligonList)<-NULL
  return(condensedPoligonList)
}
