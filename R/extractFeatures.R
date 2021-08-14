#'Some nice thing
#'
#'
#' @export
extractFeatures<-function(fn_raster,
                          fn_st_sfc,
                          fn_coverage,
                          fn_coverage_label){

  TEMP_UID<-as.character(Reduce('=',fn_st_sfc$uid))
  fn_coverage<-matrix(fn_coverage,ncol=2,byrow = T)
  rownames(fn_coverage)<-fn_coverage_label

  if (length(TEMP_UID)==0) TEMP_UID<-as.character(Reduce('=',fn_st_sfc$uid))

  TEMP_label<-fn_st_sfc$label
  TEMP_area<-sf::st_area(fn_st_sfc)
  TEMP_perimeter<-lwgeom::st_perimeter(fn_st_sfc)
  TEMP_roundness<-4*pi*TEMP_area/(TEMP_perimeter^2)
  outputTableStats<-data.frame(polygon.id=as.numeric(rownames(fn_st_sfc)),
                               label=TEMP_label,
                               area = TEMP_area,
                               perimeter = TEMP_perimeter,
                               roundness = TEMP_roundness,
                               stringsAsFactors = F)


  featuresList<-exactextractr::exact_extract(fn_raster[[TEMP_UID]],fn_st_sfc,include_xy=T)
  labels<-fn_st_sfc$label
  polygonid<-1:nrow(fn_st_sfc)
  neigbours<-matrix(c(0,1,0,-1,1,0,-1,0),ncol = 2,byrow = T)
  extraFeaturesList<-lapply(polygonid,function(pid){
    cat(paste0(pid,'...',max(polygonid),'\r'))
    estimatedCentroid<-apply(featuresList[[pid]][,c('x','y')],2,mean)
    distanceFromCentroid<-raster::pointDistance(p1 = featuresList[[pid]][,c('x','y')],
                                               p2 = estimatedCentroid,
                                               lonlat = F)

    workMatrix<-featuresList[[pid]][,c('x','y')]
    skinLayer<-vector(mode = 'numeric',length = nrow(workMatrix))
    skinLayerIndex<-0
    flag<-nrow(workMatrix)
    while(flag>0){
      testPixel<-apply(workMatrix,1,function(rr){
        scorePixel<-apply(neigbours,1,function(ngbrs){
          newNgbrs<-rr+ngbrs
          NgbrsCompare<-(newNgbrs[1]==workMatrix[,1]) & (newNgbrs[2]==workMatrix[,2])
          if (any(NgbrsCompare)) return(1) else return(0)
        })

        scorePixel<-sum(scorePixel)
        if (scorePixel<4) return(T) else return(F)
      })

      skinLayer[testPixel]<-skinLayerIndex
      workMatrix[testPixel,]<-c(Inf,Inf)
      flag<-flag-sum(testPixel)
      skinLayerIndex<-skinLayerIndex-1
    }
    skinLayer<-skinLayer-(min(skinLayer))
    data.frame(uid=TEMP_UID,
               pixel.id = 1:nrow(featuresList[[pid]]),
               polygon.id = pid,
               parLabel = labels[pid],
               label = NA,
               DFC = distanceFromCentroid,
               SLI = skinLayer,
               stringsAsFactors = F)})
  extraFeaturesList<-do.call(rbind.data.frame,c(extraFeaturesList,stringsAsFactors=F))
  featuresList<-do.call(rbind,featuresList)
  prefixes<-rownames(fn_coverage)

  newlabels<-sapply(1:nrow(featuresList),function(x){
    cf<-featuresList[x,'coverage_fraction']
    whichCf<-as.vector(apply(fn_coverage,1,function(x){cf>=min(x) & cf<=max(x)}))
    if (!any(whichCf)) {return(NA)} else { return(paste0(prefixes[whichCf],'_',extraFeaturesList[x,'parLabel']))}
  },simplify = T)
  extraFeaturesList$label<-newlabels
  featuresList<-cbind.data.frame(extraFeaturesList,featuresList,stringsAsFactors=F)
  rm(extraFeaturesList)
  featuresList<-featuresList[!is.na(featuresList$label),]
  list_out<-list(value=featuresList,geometry=outputTableStats)

  return(list_out)
}
