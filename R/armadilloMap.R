#'Some BullShit
#'
#'
#' @export
armadilloMap<-function (fn_srt,
                        fn_clpDir = c('8','4'),
                        fn_brake = 10,
                        fn_lowerQuantile=0.01,
                        fn_upperQuantile=0.50,
                        fn_lowerAreaLimit=1000,
                        fn_meanArea=100,
                        fn_populationFactor=5,
                        fn_varianceArea=1,
                        fn_lineStringBuffer = 1,
                        fn_criticalMass=40,
                        fn_PVoptimization = T,
                        fn_PVBuffer = 1.3,
                        fn_verbose = T,


                        fn_radius=10,
                        fn_Nspikes=4,
                        fn_minArea=10,
                        fn_maxArea=100,
                        fn_minRoundness=0.6,
                        fn_maxRoundness=0.8,
                        fn_coverage=0.3,
                        fn_seedOutScore=10,
                        fn_cycleWindow=1,
                        fn_discoverTreshold=10e-3,
                        fn_adaptative=T,
                        fn_areaAdaptRate=0,
                        fn_roundnessAdaptRate=0,
                        fn_segmentAlg = c('sign_simple','sign_extended','quadratic'),
                        fn_fusion=T,
                        fn_targetArea=50,
                        fn_maxNetworkSize=4,
                        fn_inflateDeflate=0.1,
                        fn_favourForeing=T,
                        fn_returnKinetic = F,
                        fn_returnRasters = F){

  drct<-as.numeric(match.arg(fn_clpDir))

  if (fn_verbose) cat('Raster pre-processing.... \n')

  globalMax<-raster::maxValue(fn_srt)
  globalMin<-raster::minValue(fn_srt)

  clippingMap<-raster::clump(x = fn_srt,
                             directions = drct,
                             gaps = F)

  clippingMap[is.na(clippingMap)]<-0
  newStars<-stars::st_as_stars(clippingMap)
  clippingPolygon<-sf::st_as_sf(newStars,merge=T)
  clippingPolygon<-clippingPolygon[clippingPolygon$clumps!=0,]
  clippingPolygon<-sf::st_make_valid(clippingPolygon)
  clippingPolygon<-sf::st_buffer(clippingPolygon,0)
  clippingPolygonArea<-sf::st_area(clippingPolygon)
  clippingPolygon<-dplyr::bind_cols(clippingPolygon,area=clippingPolygonArea)

  clippingPolygon<-clippingPolygon[clippingPolygon$area>fn_lowerAreaLimit,]

  AreaToProcess<-nrow(clippingPolygon)

  if (fn_verbose) cat(paste0('Area to process:',AreaToProcess,'\n'))

  newSegmentation<-lapply(1:AreaToProcess, function(cp){


    clpp<-sf::st_geometry(clippingPolygon[cp,])

    pixelList<-exactextractr::exact_extract(fn_srt,
                                            clpp,
                                            include_xy=T,
                                            progress = F)[[1]]

    if (fn_verbose) cat(paste0('Area ',cp,' of ',AreaToProcess,' ... extension:',
                               nrow(pixelList), ' pixels \n'))

    polyArea<-sf::st_drop_geometry(clippingPolygon[cp,'area'])
    focalNgroups<-as.numeric(round(polyArea/fn_meanArea))

    if (fn_verbose) cat(paste0('Espected polygons: ',focalNgroups,'\n'))

    pixelMax<-quantile(pixelList[,'value'],fn_upperQuantile)
    pixelMin<-quantile(pixelList[,'value'],fn_lowerQuantile)
    pixelBrake<-seq(pixelMin,pixelMax,length.out = fn_brake)
    bbBcp<-sf::st_bbox(clpp)

    pixelVoronoi<-lapply(seq_along(pixelBrake),function(nbrk){

      if (fn_verbose) cat(paste0('Iteration ',nbrk,' of ',fn_brake,'\r'))


      ########alternative #####

      newPixelList<-pixelList
      newPixelList$value<-NA
      newPixelList[pixelList$value<=pixelBrake[nbrk],'value']<-1
      newSrt<-raster::raster(matrix(NA,bbBcp[4]-bbBcp[2],bbBcp[3]-bbBcp[1]),
                             xmn=bbBcp[1],
                             xmx=bbBcp[3],
                             ymn=bbBcp[2],
                             ymx=bbBcp[4],
                             crs=NA)
      newPixelCell<-raster::cellFromXY(newSrt,newPixelList[,c('x','y')])
      newSrt[newPixelCell]<-newPixelList[,'value']
      newSrt<-raster::clump(x = newSrt,
                            directions = drct,
                            gaps = F)
      newPixelList<-exactextractr::exact_extract(newSrt,
                                                 clpp,
                                                 include_xy=T,
                                                 progress = F)[[1]]

      focalPixelList_x<-aggregate(x~value,newPixelList,mean)
      focalPixelList_y<-aggregate(y~value,newPixelList,mean)
      focalPixelList<-dplyr::inner_join(focalPixelList_x,
                                        focalPixelList_y,
                                        by='value')

      ####alternative - end

      # focalPixelList<-pixelList[pixelList$value<=pixelBrake[nbrk],]
      # raster::plot(fn_srt<=nbrk,xlim=c(bbBcp[[1]],bbBcp[[3]]),ylim=c(bbBcp[[2]],bbBcp[[4]]))

      if (nrow(focalPixelList)<2){

        return(clpp)
      } else {

        if (nrow(focalPixelList)>focalNgroups & focalNgroups>0){
          focalPixelDist<-dist(focalPixelList[,c('x','y')])
          focalPixelDendro<-hclust(focalPixelDist,method = 'centroid')

          focalPixelGroups<-cutree(focalPixelDendro,k=focalNgroups)
          focalPoint<-lapply(1:focalNgroups,function(fpg){
            x<-mean(focalPixelList[focalPixelGroups==fpg,'x'])
            y<-mean(focalPixelList[focalPixelGroups==fpg,'y'])
            return(c(x,y))
          })
          focalPoint<-do.call(rbind,focalPoint)
          focalPoint<-sf::st_multipoint(focalPoint)
        } else {
          focalPoint<-sf::st_multipoint(as.matrix(focalPixelList[,c('x','y')]))
        }

        if (length(focalPoint)==2){
          return(clpp)
        }


        #
        voroSkeleton<-sf::st_voronoi(focalPoint,envelope = sf::st_geometry(clpp))
        if (any(sf::st_geometry_type(voroSkeleton)=='GEOMETRYCOLLECTION')){
          voroSkeleton<-sf::st_collection_extract(voroSkeleton)
        }
        voroSkeleton<-sf::st_buffer(voroSkeleton,0)

        # voroCover<-sf::st_contains(clpp,voroSkeleton)[[1]]
        # voroSkeleton_toCut<-voroSkeleton[-voroCover]
        # voroSkeleton_toLeave<-voroSkeleton[voroCover]

        voroSkeleton<-sf::st_intersection(voroSkeleton,clpp)

        # voroSkeleton<-c(voroSkeleton,voroSkeleton_toLeave)

        if (any(sf::st_geometry_type(voroSkeleton)=='GEOMETRYCOLLECTION')){
          voroSkeleton<-sf::st_collection_extract(voroSkeleton)
        }

        if (any(sf::st_geometry_type(voroSkeleton)=='MULTIPOLYGON')){
          voroSkeleton<-sf::st_cast(voroSkeleton,
                                    'MULTIPOLYGON',
                                    group_or_split = T,
                                    do_split = T)
          voroSkeleton<-sf::st_cast(voroSkeleton,
                                    'POLYGON',
                                    group_or_split = T,
                                    do_split = T)
        }


        if (!inherits(voroSkeleton,'sfc_POLYGON')){
          out<-sf::st_sfc(voroSkeleton)
        } else {
          out<-voroSkeleton
        }

      }


      # raster::plot(fn_srt,xlim=c(bbBcp[[1]],bbBcp[[3]]),ylim=c(bbBcp[[2]],bbBcp[[4]]))
      # plot(out,add=T)
      return(out)
    })

    if (fn_verbose) cat(paste0('Completed ',fn_brake,' iterations \n'))
    if (fn_verbose) cat(paste0('Evaluating polygon compositions ... \n'))
    if (fn_verbose) cat(paste0('Area '))

    areaPoly<-lapply(pixelVoronoi,function(x){

      if (fn_verbose) cat(paste0('.'))
      if (inherits(x,'sfc')){
        out<-sf::st_area(x)
      } else {
        out<-0
      }
      return(out)
    })

    if (fn_verbose) cat(paste0('OK \n'))

    variancePoly<-sapply(areaPoly,var,simplify = T,USE.NAMES = F)

    # clippingLinestring<-sf::st_cast(sf::st_geometry(clpp),
    #                                 'LINESTRING',
    #                                 group_or_split = T,
    #                                 do_split = T)

    if (fn_verbose) cat(paste0('Borders '))

    # lineStringCast<-lapply(pixelVoronoi,function(x){
    #
    #   if (fn_verbose) cat(paste0('.'))
    #   if (inherits(x,'sfc')){
    #     if (any(sf::st_geometry_type(x)=='GEOMETRYCOLLECTION')){
    #       out<-sf::st_collection_extract(x)
    #     } else {
    #       out<-x
    #     }
    #     out<-sf::st_cast(out,
    #                      'POLYGON',
    #                      group_or_split = T,
    #                      do_split = T)
    #     out<-sf::st_cast(out,
    #                      'LINESTRING',
    #                      group_or_split = T,
    #                      do_split = T)
    #     out<-nngeo::st_segments(out,progress = F)
    #
    #     borderDelimiter<-sf::st_covered_by(out,clippingLinestring)
    #     wbdl<-sapply(borderDelimiter,
    #                  function(x){length(x)==0 },simplify = T,USE.NAMES = F)
    #     out<-out[wbdl,]
    #   } else {
    #     out<-sf::st_sfc(NULL)
    #   }
    #   return(out)
    # })

    ####alternative Border#####

    varianceBorder<-sapply(pixelVoronoi,function(x){

      if (fn_verbose) cat(paste0('.'))
      if (inherits(x,'sfc')){
        if (any(sf::st_geometry_type(x)=='GEOMETRYCOLLECTION')){
          out<-sf::st_collection_extract(x)
        } else {
          out<-x
        }
        out<-sf::st_cast(out,
                         'POLYGON',
                         group_or_split = T,
                         do_split = T)
        out<-exactextractr::exact_extract(fn_srt,out,include_XY=F,progress=F)

        out<-sapply(out,function(o){
          o<-o[o[,'coverage_fraction']<1,'value']
          if (length(o)>1) out<-var(o) else out<-0
        },simplify = T,USE.NAMES = F)
        out<-mean(out)
      } else {
        out<-NA
      }
      return(out)
    },simplify = T,USE.NAMES = F)

    # varianceBorder<-sapply(lineStringCast,function(x){
    #   if (!all(sf::st_is_empty(x))){
    #
    #     # out<-raster::extract(fn_srt,
    #     #                      sf::st_sf(x),
    #     #                      progress = 'none')
    #
    #     out<-exactextractr::exact_extract(fn_srt,
    #                                       sf::st_buffer(x,fn_lineStringBuffer),
    #                                       progress = F)
    #     out<-lapply(out,function(x)x[,1,drop=T])
    #     out<-lapply(out,na.omit)
    #     out<-lapply(out,var)
    #     out<-lapply(out,na.omit)
    #     wout<-sapply(out,function(x) length(x)!=0,simplify = T,USE.NAMES = F)
    #     out<-out[wout]
    #     out<-do.call(mean,out)
    #   } else {
    #     out<-Inf}
    #   return(out)
    # },simplify = T,USE.NAMES = F)

    if (fn_verbose) cat(paste0('OK \n'))

    variancePoly<-(variancePoly-fn_varianceArea)^2
    rankVpoly<-rank(variancePoly,na.last = T,ties.method = 'average')
    rankVBorder<-rank(varianceBorder,na.last = T,ties.method = 'average')
    score<-rankVpoly^2+rankVBorder^2
    rankScore<-rank(score,na.last = T,ties.method = 'average')
    out<-pixelVoronoi[[which.min(rankScore)]]

    if (fn_PVoptimization) {

      optiPoly<-lapply(1:length(out),function(no){

        newOut<-sf::st_segmentize(out[no],1)
        newOut_coods<-sf::st_coordinates(newOut)
        centroidOut<-sf::st_centroid(newOut)
        centroidOut_coords<-sf::st_coordinates(centroidOut)
        newOutBufferOuter<-((newOut-centroidOut_coords)*fn_PVBuffer)+centroidOut_coords
        newOutBufferInner<-((newOut-centroidOut_coords)/fn_PVBuffer)+centroidOut_coords
        newOutBufferOuter_coords<-sf::st_coordinates(newOutBufferOuter)
        newOutBufferInner_coords<-sf::st_coordinates(newOutBufferInner)
        newSkeleton<-lapply(1:nrow(newOutBufferOuter_coords),function(nr){
          out_coords<-rbind(
            newOutBufferInner_coords[nr,c('X','Y')],
            newOut_coods[nr,c('X','Y')],
            newOutBufferOuter_coords[nr,c('X','Y')])
          out<-sf::st_linestring(out_coords)
          out<-sf::st_segmentize(out,1)
          out<-sf::st_coordinates(out)
          out_values<-raster::cellFromXY(fn_srt,out)
          out_values<-fn_srt[out_values]
          w_value<-which.max(out_values)

          return(out[w_value,])
        })
        newSkeleton<-do.call(rbind,newSkeleton)[,c('X','Y')]
        newSkeleton<-round(newSkeleton)
        newSkeleton<-rbind(newSkeleton,newSkeleton[1,])

        out<-sf::st_polygon(list(newSkeleton))
        return(out)
      })
      optiOut<-sf::st_sfc(optiPoly)
    }



    # raster::plot(fn_srt,xlim=c(bbBcp[[1]],bbBcp[[3]]),ylim=c(bbBcp[[2]],bbBcp[[4]]))
    # plot(out,add=T)
    # plot(optiOut,add=T,border='blue')
    return(out)
  })


  return(newSegmentation)
}
#   if (is.character(fn_targetArea)){
#     if (!any(fn_targetArea == c('predicted_mean','predicted_median','predicted_mode','predicted_max','predicted_middle'))) stop(RUNIMC:::mError(('fn_targetArea should be a single numeric or "predicted_mean", "predicted_median, "predicted_mode", "predicted_max", "predicted_middle"')))
#   }
#
#   fn_segmentAlg<-match.arg(fn_segmentAlg)
#
#   TimingFunction<-data.frame(Nseeds=0,Npoly=0,Ntime=Sys.time())
#
#   xmn<-fn_srt@extent[1]
#   xmx<-fn_srt@extent[2]
#   ymn<-fn_srt@extent[3]
#   ymx<-fn_srt@extent[4]
#
#   #
#   DFraster<-raster::as.data.frame(fn_srt,xy=T)
#   colnames(DFraster)<-c('x','y','value')
#   cellIndex<-raster::cellFromXY(fn_srt,DFraster[,c('x','y')])
#   DFraster<-data.frame(cellIndex=cellIndex,DFraster,stringsAsFactors = F)
#   DFraster<-DFraster[!is.na(DFraster$value),]
#   DFraster<-DFraster[order(DFraster$value,decreasing = F),]
#
#   bunchOfSeeds<-data.frame(seed= DFraster$cellIndex,
#                            score=rep(0,nrow(DFraster)),
#                            active=rep(1,nrow(DFraster)))
#   rm(DFraster)
#   lyr<-names(fn_srt)
#
#   if (nrow(bunchOfSeeds)==0){
#     if (!fn_returnRasters) fn_srt<-raster::raster(matrix(0))
#     fakePolygon<-matrix(data = c(xmn,ymn,xmn+1,ymn,xmn+1,ymn+1,xmn,ymn+1,xmn,ymn),
#                         ncol = 2,
#                         byrow = T)
#     colnames(fakePolygon)<-c('x','y')
#     segmentationOut<-new('IMC_Segmentation',
#                          polygons=list(fakePolygon),
#                          performance=data.frame(Nseeds=numeric(0),
#                                                 Npoly=numeric(0),
#                                                 Ntime=numeric(0))
#                          ,raster=fn_srt)
#     return(segmentationOut)
#   }
#
#   polyList<-vector(mode = 'list',length = nrow(bunchOfSeeds))
#   areaList<-vector(mode = 'list',length = nrow(bunchOfSeeds))
#
#   rMsk<-RUNIMC:::radialMask(fn_radius,fn_Nspikes)
#   sinMatrix<-rMsk$sinMatrix
#   cosMatrix<-rMsk$cosMatrix
#   intersectionMatrix<-rMsk$intersectionMatrix
#
#   polyIndex=0
#   cycleIndex=1
#   oldCycleIndex=1
#   oldPolyIndex=0
#   oldSeedIndex=0
#   seedPointer=1
#   activeSeed<-c()
#   switchOff<-sum(bunchOfSeeds$active)
#   cycleWindowProportion<-fn_cycleWindow/1000
#
#   while(switchOff>0){
#
#
#     logicActive<-bunchOfSeeds$active==1
#     minPointerScore<-min(bunchOfSeeds$score[logicActive])
#     logicScore<-bunchOfSeeds$score==minPointerScore
#     seedPointer<-which(logicActive & logicScore)[1]
#     bunchOfSeeds$score[seedPointer]<-bunchOfSeeds$score[seedPointer]+1
#     activeSeed<-bunchOfSeeds$seed[seedPointer]
#
#
#     cat(paste0(lyr,' / ',
#                'N of seeds: ',formatC(switchOff,flag='0',digits = 9),
#                ' / N of polygons: ',formatC(polyIndex,flag='0',digits = 9),
#                ' / cycleIndex: ', formatC(cycleIndex,flag='0',digits=9),
#                # ' / discovery window |',paste0(rep("@",TEMP_windowFraction),collapse = ""),
#                # paste0(rep("_",TEMP_remaining),collapse = ""),
#                '| / throwout after: ',
#                formatC(fn_seedOutScore,flag='0',digits=3),
#                ' / Area range: ',
#                formatC(fn_minArea,flag=' ',digits=3),
#                ' % ',
#                formatC(fn_maxArea,flag=' ',digits=3),
#                ' / Roundness range: ',
#                formatC(fn_minRoundness,flag=' ',digits=3),
#                ' % ',
#                formatC(fn_maxRoundness,flag=' ',digits=3),
#                '\r'))
#
#     TEMP<-raster::xyFromCell(fn_srt,activeSeed)
#
#     xCoords<-TEMP[1]
#     yCoords<-TEMP[2]
#
#     xScan<-sinMatrix+xCoords
#     yScan<-cosMatrix+yCoords
#
#     xScan[xScan<xmn]<-NA
#     yScan[yScan<ymn]<-NA
#     xScan[xScan>xmx]<-NA
#     yScan[yScan>ymx]<-NA
#     xyScan<-matrix(1:length(xScan),ncol=ncol(xScan),nrow=nrow(xScan))
#
#     xyVal<-apply(xyScan, c(1,2),function(x){
#       ifelse((is.na(xScan[[x]]) | is.na(yScan[[x]])),
#              NA,
#              fn_srt[raster::cellFromXY(fn_srt,c(xScan[[x]],yScan[[x]]))]
#       )
#     })
#
#     orgVal<-fn_srt[raster::cellFromXY(fn_srt,c(xCoords,yCoords))]
#
#
#     switch(fn_segmentAlg,
#            sign_simple = {
#              spikeEnds<-apply(xyVal,1,.segmentBySign_simple,orgVal)
#            },
#            sign_extended = {
#              spikeEnds<-apply(xyVal,1,.segmentBySign_extended,orgVal,fn_minArea,fn_maxArea)
#            },
#            quadratic = {
#              bigX<-matrix(c(rep(1,fn_radius),(1:fn_radius)^2,1:fn_radius),byrow = F,ncol = 3)
#              spikeEnds<-apply(xyVal,1,.segmentByQuadratic,orgVal,fn_favourForeing,bigX)
#            })
#
#
#
#     polyG<-try(t(sapply(1:fn_Nspikes,function(x){
#       intersectionMatrix[x,spikeEnds[1,x]][[1]][spikeEnds[2,x]+1,]+c(xCoords,yCoords)
#     })))
#
#     if (inherits(polyG,'try-error'))
#     colnames(polyG)<-c('x','y')
#
#     polyG<-na.omit(polyG)
#     notDup<-!duplicated(polyG)
#     polyG<-polyG[notDup,,drop=F]
#
#     if (!(nrow(polyG)<3)){
#       polyG<-rbind(polyG,polyG[1,])
#       polyArea<-RUNIMC:::.area(polyG)
#       polyPerimeter<-RUNIMC:::.perimeter(polyG)
#       polyRoundness<-RUNIMC:::.roundness(polyArea,polyPerimeter)
#
#       if (fn_returnKinetic) TimingFunction<-rbind(TimingFunction,data.frame(Nseeds=nrow(bunchOfSeeds),Npoly=polyIndex,Ntime=Sys.time()))
#       if (polyArea>=fn_minArea & polyArea<=fn_maxArea & polyRoundness>=fn_minRoundness & polyRoundness<=fn_maxRoundness){
#         sfPolygon<-sf::st_polygon(list(as.matrix(polyG)),dim="XY")
#         coverageDF <- (exactextractr::exact_extract(fn_srt,sf::st_sfc(sfPolygon),include_xy=T))[[1]]
#         coverageDF<-coverageDF[coverageDF$coverage_fraction>=fn_coverage,]
#         coordDF<-raster::cellFromXY(fn_srt,as.matrix(coverageDF[,c(2,3)]))
#         bunchOfSeeds$active[bunchOfSeeds$seed %in% coordDF]<-0
#         polyIndex=polyIndex+1
#         fn_srt[coordDF]<-(-polyIndex)
#         polyList[[polyIndex]]<-sfPolygon
#         areaList[[polyIndex]]<-polyArea
#       }
#     }
#
#
#     if ((cycleIndex-oldCycleIndex)>switchOff*cycleWindowProportion){
#
#       if (polyIndex==oldPolyIndex & fn_adaptative==F) {break()}
#
#       if (polyIndex==oldPolyIndex & fn_adaptative==T & fn_seedOutScore>1) {
#         fn_seedOutScore=fn_seedOutScore-1
#         fn_minArea<-fn_minArea-(fn_minArea*fn_areaAdaptRate)
#         fn_maxArea<-fn_maxArea+(fn_maxArea*fn_areaAdaptRate)
#         fn_minRoundness<-fn_minRoundness-(fn_minRoundness*fn_roundnessAdaptRate)
#         fn_maxRoundness<-fn_maxRoundness+(fn_maxRoundness*fn_roundnessAdaptRate)
#       }
#       oldCycleIndex=cycleIndex
#       oldPolyIndex=polyIndex
#     }
#
#     bunchOfSeeds$active[bunchOfSeeds$score>fn_seedOutScore]<-0
#     switchOff<-sum(bunchOfSeeds$active)
#     cycleIndex=cycleIndex+1
#   }
#
#   cat(paste0(lyr,' / ',
#              'N of seeds: ',formatC(sum(bunchOfSeeds$active),flag='0',digits = 9),
#              ' / N of polygons: ',formatC(polyIndex,flag='0',digits = 9),
#              ' / cycleIndex: ', formatC(cycleIndex,flag='0',digits=9),
#              # ' / discovery window |',paste0(rep("@",TEMP_windowFraction),collapse = ""),
#              # paste0(rep("_",TEMP_remaining),collapse = ""),
#              '| / throwout after: ',
#              formatC(fn_seedOutScore,flag='0',digits=3),
#              ' / Area range: ',
#              formatC(fn_minArea,flag=' ',digits=3),
#              ' % ',
#              formatC(fn_maxArea,flag=' ',digits=3),
#              ' / Roundness range: ',
#              formatC(fn_minRoundness,flag=' ',digits=3),
#              ' % ',
#              formatC(fn_maxRoundness,flag=' ',digits=3),
#              '\n'))
#
#   polyList<-polyList[1:polyIndex]
#   areaList<-areaList[1:polyIndex]
#
#
#   if (fn_fusion){
#
#     polyFSC<-sf::st_sfc(polyList)
#     polyIntersection<-sf::st_intersects(polyFSC,sparse=F)
#
#     if (is.character(fn_targetArea)){
#       fn_targetArea
#       switch(fn_targetArea,
#              predicted_mean = {
#                fn_targetArea<-mean(unlist(areaList))
#              },
#              predicted_median = {
#                fn_targetArea<-median(unlist(areaList))
#              },
#              predicted_mode = {
#                brks<-0:ceiling(unlist(areaList))
#                frqT<-hist(x = unlist(areaList),breaks = brks)
#                fn_targetArea<-median(frqT$breaks[which(frqT$counts==max(frqT$counts))])
#              },
#              predicted_max = {
#                fn_targetArea<-max(unlist(areaList))
#              },
#              predicted_middle = {
#                fn_targetArea<-(max(unlist(areaList))-min(unlist(areaList)))/2
#              })
#     }
#
#     nt<-igraph::graph_from_adjacency_matrix(adjmatrix = polyIntersection,mode = 'undirected',diag = F)
#     igraph::V(nt)$name<-as.character(1:nrow(polyIntersection))
#     nt_edgeList<-igraph::as_edgelist(nt,names=T)
#     nt_components<-igraph::components(nt)
#     nt_subgraphIndexVector<-igraph::membership(nt_components)
#     nt_subgraphIndices<-unique(nt_subgraphIndexVector)
#     nt_subgraph<-lapply(nt_subgraphIndices,function(i){
#       out<-igraph::induced_subgraph(nt,names(nt_subgraphIndexVector[nt_subgraphIndexVector==i]))
#       return(out)
#     })
#     nt_length<-lapply(nt_subgraph,igraph::gsize)
#     nt_maxLength<-max(unlist(nt_length))
#
#     while(nt_maxLength>fn_maxNetworkSize){
#       TEMP<-lapply(nt_subgraph,function(sbg){
#         nt_sbgLength<-igraph::gsize(sbg)
#         if (nt_sbgLength>fn_maxNetworkSize){
#           newCommunity<-igraph::edge.betweenness.community(sbg)
#           newMembers<-igraph::membership(newCommunity)
#           newMembersIndex<-unique(newMembers)
#           newSubgraph<-lapply(newMembersIndex,function(i){
#             newCommunityVertex<-names(newMembers[newMembers==i])
#             newSubSub<-igraph::induced_subgraph(nt,newCommunityVertex)
#             return(newSubSub)
#           })
#         } else (newSubgraph<-sbg)
#       })
#       newListSubgraph<-list()
#       newLSIndex<-1
#       for (i in 1:length(TEMP)){
#         if(class(TEMP[[i]])=='list'){
#           for (ii in 1:length(TEMP[[i]])){
#             newListSubgraph[[newLSIndex]]<-TEMP[[i]][[ii]]
#             newLSIndex<-newLSIndex+1
#           }} else{
#             newListSubgraph[[newLSIndex]]<-TEMP[[i]]
#             newLSIndex<-newLSIndex+1
#           }
#       }
#       nt_subgraph<-newListSubgraph
#       nt_length<-lapply(nt_subgraph,igraph::gsize)
#       newMaxLength<-max(unlist(nt_length))
#       if (newMaxLength==nt_maxLength) {nt_maxLength<-0} else {nt_maxLength<-newMaxLength}
#     }
#
#     newPolyList<-lapply(nt_subgraph,function(nts){
#
#       nt_edges<-igraph::as_edgelist(nts)
#       if (nrow(nt_edges)>0){
#
#         nt_vertex<-unique(as.vector(nt_edges))
#         nt_permutations<-t(expand.grid(rep(list(c(T,F)),nrow(nt_edges))))
#
#         new_deltaPermutations<-pbapply::pbapply(nt_permutations,2,function(prmt){
#
#           new_edges<-nt_edges[prmt,,drop=F]
#           new_vertex<-nt_vertex[!(nt_vertex %in% unique(as.vector(new_edges)))]
#           new_emptyEdges<-t(sapply(new_vertex,function(x){c(x,NA)},USE.NAMES = F,simplify = T))
#           new_subgraph<-igraph::graph_from_edgelist(new_edges, directed = F)
#           new_subgraph<-igraph::add_vertices(new_subgraph,length(new_vertex),name=new_vertex)
#           new_components<-igraph::components(new_subgraph)
#           new_groups<-sapply(unique(new_components$membership),function(x){
#             polygonsInGroup<-names(new_components$membership[new_components$membership==x])
#           },USE.NAMES = T,simplify = F)
#           new_area<-lapply(new_groups,function(x){
#             sum(unlist(areaList[as.numeric(x)]))
#           })
#
#           new_cumulativeDelta<-sum((unlist(new_area)-fn_targetArea)^2)
#           return(list(area=new_cumulativeDelta,graph=new_subgraph))
#
#         })
#
#         mm<-lapply(new_deltaPermutations,function(x)x$area)
#         mm<-which(mm==min(unlist(mm)))[1]
#         winnerPermutation<-new_deltaPermutations[[mm]]$graph
#         winnerComponents<-igraph::components(winnerPermutation)
#         winnerPolygons<-sapply(unique(winnerComponents$membership),function(x){
#           polygonsInGroup<-names(winnerComponents$membership[winnerComponents$membership==x])
#           unionPolygon<-sf::st_union(sf::st_sfc(polyList[as.numeric(polygonsInGroup)]))
#           unionPolygon<-sf::st_buffer(unionPolygon,fn_inflateDeflate)
#           unionPolygon<-sf::st_buffer(unionPolygon,-fn_inflateDeflate)
#           unionPolygon<-nngeo::st_remove_holes(unionPolygon)
#           return(unionPolygon)})
#
#         return (winnerPolygons)
#       } else (return(polyList[[as.numeric(igraph::V(nts)$name)]]))
#
#     })
#     newPolyList<-RUNIMC:::.wiseUnlist(newPolyList)
#
#     newPolyList<-lapply(newPolyList,function(x){
#       if (class(x)=='list') {return(x[[1]])} else (return(x))
#     })
#
#   } else { newPolyList<-unlist(sf::st_sfc(polyList),recursive = F)}
#
#   if (fn_returnKinetic) TimingFunction<-rbind(TimingFunction,data.frame(Nseeds=nrow(bunchOfSeeds),Npoly=polyIndex,Ntime=Sys.time()))
#   if (!fn_returnRasters) fn_srt<-raster::raster(matrix(0))
#
#   segmentationOut<-new('IMC_Segmentation',polygons=newPolyList,performance=TimingFunction,raster=fn_srt)
#   return(segmentationOut)
# }
#
#
# #### prototype with sum bugs
# # .segmentBySign<-function(profileVector,orgVal){
# #   profileVector[profileVector<0]<-0
# #   profileVector[is.na(profileVector)]<-0
# #   shiftVector<-c(orgVal,profileVector[-length(profileVector)])
# #   signVector<-sign(profileVector-shiftVector)
# #   zeroSign<-which(signVector==0)
# #   zeroSubstitute<-zeroSign-1
# #   zeroSubstitute[zeroSubstitute==0]<-2
# #   signVector[zeroSign]<-signVector[zeroSubstitute]
# #   runLength<-rle(signVector)
# #   runLength$lengths<-cumsum(runLength$lengths)
# #   topS<-profileVector[runLength$lengths[runLength$values == 1]]
# #   bottomS<-profileVector[runLength$lengths[runLength$values == -1]]
# #   T_winner<-topS/c(orgVal,bottomS[(1:length(topS))-1])
# #   winner<-which.max(T_winner)
# #   winner<-runLength$lengths[runLength$values == 1][winner]
# #   if (length(winner)==0) winner<-length(profileVector)
# #   if (any(profileVector[1:winner]==0)){
# #     winner<-which(profileVector[1:winner]==0)[1]
# #     return(list=c(winner,0))
# #   }
# #   return(list=c(winner,1))
# # }
#
# #### chose peck with larger peck to valley ratio
# .segmentBySign_simple<-function(profileVector,orgVal){
#
#   profileVector[profileVector<0]<-0
#   profileVector[is.na(profileVector)]<-0
#   shiftVector<-c(orgVal,profileVector[-length(profileVector)])
#   signVector<-sign(profileVector-shiftVector)
#   signVector_1<-sign(profileVector-shiftVector)
#   zeroSign<-which(signVector==0)
#
#   if (length(zeroSign)>0){
#
#     for (i in zeroSign){
#       ifelse(i==1 & length(signVector)>=2,
#              {signVector_1[i] = signVector[i+1]},
#              {
#                plus<-which(signVector[1:i]==1)
#                if (length(plus)>1) plus<-max(plus)
#                minus<-which(signVector[1:i]==-1)
#                if (length(minus)>1) minus<-max(minus)
#                if (length(plus)==0 & length(minus)==0) return(list=c(1,0))
#                pm<-max(c(plus,minus))
#                if (signVector[pm]==1) signVector_1[i]=1 else signVector_1[i]=-1
#
#              })
#     }
#   }
#
#   runLength<-rle(signVector_1)
#   runLength$lengths<-cumsum(runLength$lengths)
#   topS<-profileVector[runLength$lengths[runLength$values == 1]]
#   bottomS<-profileVector[runLength$lengths[runLength$values == -1]]
#
#   if (runLength$values[1]==1) dnmntr<- c(orgVal,bottomS[(1:length(topS))-1]) else dnmntr<-bottomS[(1:length(topS))]
#   T_winner<-topS/dnmntr
#   winner<-which.max(T_winner)
#   winner<-runLength$lengths[runLength$values == 1][winner]
#   if (length(winner)==0) winner<-length(profileVector)
#   if (any(profileVector[1:winner]==0)){
#     winner<-which(profileVector[1:winner]==0)[1]
#     return(list=c(winner,0))
#   }
#   return(list=c(winner,1))
# }
#
#
# #### chose peak with largest peack to valley ratio and lower distance from
# #### expected radius
# .segmentBySign_extended<-function(profileVector,orgVal,minA,maxA){
#
#   profileVector[profileVector<0]<-0
#   profileVector[is.na(profileVector)]<-0
#   shiftVector<-c(orgVal,profileVector[-length(profileVector)])
#   signVector<-sign(profileVector-shiftVector)
#   signVector_1<-sign(profileVector-shiftVector)
#   zeroSign<-which(signVector==0)
#
#   if (length(zeroSign)>0){
#
#     for (i in zeroSign){
#       ifelse(i==1 & length(signVector)>=2,
#              {signVector_1[i] = signVector[i+1]},
#              {
#                plus<-which(signVector[1:i]==1)
#                if (length(plus)>1) plus<-max(plus)
#                minus<-which(signVector[1:i]==-1)
#                if (length(minus)>1) minus<-max(minus)
#                if (length(plus)==0 & length(minus)==0) return(list=c(1,0))
#                pm<-max(c(plus,minus))
#                if (signVector[pm]==1) signVector_1[i]=1 else signVector_1[i]=-1
#
#              })
#     }
#   }
#
#     runLength<-rle(signVector_1)
#   runLength$lengths<-cumsum(runLength$lengths)
#   topS<-profileVector[runLength$lengths[runLength$values == 1]]
#   topS_area<-abs(((pi*topS^2)-((maxA-minA)/2)))
#
#   bottomS<-profileVector[runLength$lengths[runLength$values == -1]]
#
#   if (runLength$values[1]==1) dnmntr<- c(orgVal,bottomS[(1:length(topS))-1]) else dnmntr<-bottomS[(1:length(topS))]
#   T_winner<-topS/dnmntr
#   T_winner<-T_winner/topS_area
#   winner<-which.max(T_winner)
#   winner<-runLength$lengths[runLength$values == 1][winner]
#   if (length(winner)==0) winner<-length(profileVector)
#   if (any(profileVector[1:winner]==0)){
#     winner<-which(profileVector[1:winner]==0)[1]
#     return(list=c(winner,0))
#   }
#   return(list=c(winner,1))
# }
#
#
# ###chose peack as maximum of a quadratic curve
# .segmentByQuadratic<-function(profileVector,orgVal,fn_favourForeing,bigX){
#
#   profileVector[profileVector<0]<-0
#   profileVector[is.na(profileVector)]<-0
#   if (fn_favourForeing & any(profileVector==0)){
#     peakRound<-which(profileVector==0)[1]
#     u<-0
#     return(list=c(peakRound,u))
#   } else {
#
#     fitPoly<-.lm.fit(bigX,profileVector)
#     a <- coef(fitPoly)[2]
#     b <- coef(fitPoly)[3]
#     peak<-(-(b/(2*a)))
#     peakRound<-round(peak)
#     hillSide<-peak-peakRound
#
#     if (is.null(peakRound) | is.na(peakRound) | length(peakRound)==0){
#       peakRound<-0
#       hillside<-0
#     }
#     if (peakRound>length(profileVector)) peakRound<-length(profileVector)
#     if (peakRound<1) {
#       if (any(profileVector==0)){
#         peakRound<-min(which(profileVector==0))[1]
#         hillSide<-0
#       } else {
#         peakRound<-length(peakRound)
#         hillSide<-1}
#     }
#     if (any(profileVector==0)){
#       if (peakRound>min(which(profileVector==0))[1]){
#         peakRound<-min(which(profileVector==0))[1]
#         hillSide<-0
#       }
#     }
#     if (hillSide<=0.5) u<-0 else u<-1
#     return(list=c(peakRound,u))}
# }
