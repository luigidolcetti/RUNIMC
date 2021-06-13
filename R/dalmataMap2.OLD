#'Some BullShit
#'
#'
#' @export
dalmataMap<-function (fn_srt,
                      fn_clpDir = c('8','4'),
                      fn_brake = 10,
                      fn_lowerQuantile=0.01,
                      fn_upperQuantile=0.50,
                      fn_lowerAreaLimit=5,
                      fn_upperAreaLimit=50,
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

  pixelList<-raster::values(fn_srt)
  pixelMax<-quantile(pixelList,fn_upperQuantile,na.rm=T)
  pixelMin<-quantile(pixelList,fn_lowerQuantile,na.rm=T)
  pixelBrake<-seq(pixelMin,pixelMax,length.out = fn_brake)
  maxii<-length(pixelBrake)


  polyLayer<-lapply(seq_along(pixelBrake),function(ii){
    if (fn_verbose) cat('Layer : ',ii,' of ',maxii,'\r')
    clippingMap<-fn_srt<=pixelBrake[ii]
    clippingMap<-raster::clump(x = clippingMap,
                               directions = drct,
                               gaps = F)
    # raster::plot(clippingMap)
    newStars<-stars::st_as_stars(clippingMap)
    clippingPolygon<-sf::st_as_sf(newStars,merge=T)
    clippingPolygon<-sf::st_make_valid(clippingPolygon)
    clippingPolygon<-sf::st_buffer(clippingPolygon,0)
    clippingPolygonArea<-sf::st_area(clippingPolygon)
    clippingPolygon<-dplyr::bind_cols(clippingPolygon,area=clippingPolygonArea)
    return(clippingPolygon)
    # plot(clippingPolygon[1],col=NA,add=T)
  })
  if (fn_verbose) cat('Layer scan completed\n')
  if (fn_verbose) cat('Constructing network\n')

  maxii<-length(polyLayer)
  pL<-seq_along(polyLayer)[-1]
  polyEdge<-lapply(pL,function(ii){
    # if (fn_verbose) cat('Edges : ',ii,' of ',maxii,'\r')
    edgeDef<-sf::st_within(polyLayer[[ii-1]],polyLayer[[ii]])
    maxiii<-length(edgeDef)
    edgeBrake<-lapply(1:length(edgeDef),function(iii){
      if (fn_verbose) cat('Edges : ',ii,' of ',maxii,'... element: ',iii, ' of ',maxiii,'\r')
      to<-paste0(ii-1,'.',iii)
      if (length(edgeDef[[iii]])>0){
        from<-paste0(ii,'.',edgeDef[[iii]])
      } else {
        to<-NA
      }
      out<-data.frame(from,to,stringsAsFactors = F)
    })

    out<-do.call(rbind,edgeBrake)
  })
  if (fn_verbose) cat('Edges : done...\n')
  polyEdge<-do.call(rbind,polyEdge)

  if (fn_verbose) cat('Making big network\n')

  nodeList<-unique(c(polyEdge$from,polyEdge$to))
  nodeIndex<-strsplit(nodeList,'.',fixed=T)
  nodeIndex<-do.call(rbind,nodeIndex)
  nodeIndex<-matrix(as.numeric(nodeIndex),ncol = 2,byrow = F)
  polyArea<-lapply(polyLayer,function(x) sf::st_drop_geometry(x))
  areaList<-apply(nodeIndex,1,function(idx){
    out<-polyArea[[idx[1]]][idx[2],'area',drop=T]
    if (!is.numeric(out)) out<-0
    return(out)
  })


  nodeDataFrame<-data.frame(ID=nodeList,
                            layerIndex = nodeIndex[,1],
                            polyIndex = nodeIndex[,2],
                            area = areaList,
                            stringsAsFactors = F)


  edgeList<-polyEdge[!is.na(polyEdge$to),]

  polyNet<-igraph::graph_from_data_frame(edgeList, directed=TRUE, vertices=nodeDataFrame)

  subgroups<-igraph::decompose.graph(polyNet)

  chunks<-lapply(subgroups,function(sgrp){
    browser()
plot(sgrp)
  })

  if (fn_verbose) cat('Selecting pixel-seeds\n')

  seedList<-lapply(subgroups,function(sgrp){

    newSubGroup<-igraph::delete.vertices(sgrp, igraph::V(sgrp)[ igraph::V(sgrp)[area >fn_upperAreaLimit]])
    newSubGroup<-igraph::delete.vertices(newSubGroup, igraph::V(newSubGroup)[ igraph::V(newSubGroup)[area <fn_lowerAreaLimit]])
    newSplit<-igraph::decompose.graph(newSubGroup)
    maxPoly<-lapply(newSplit,function(nwsplt){

      as.data.frame(igraph::vertex.attributes(nwsplt,index = igraph::V(nwsplt)[ which.max(area) ]))
    })
    maxPoly<-do.call(rbind.data.frame,maxPoly)
  })

  seedList<-do.call(rbind.data.frame,seedList)
  seedList<-lapply(1:nrow(seedList),function(ii){
    lyr<-seedList$layerIndex[ii]
    rw<-seedList$polyIndex[ii]
    polyLayer[[lyr]][rw,]
  })
  seedList<-do.call(dplyr::bind_rows,seedList)
  maskList<-exactextractr::exact_extract(fn_srt,
                                         seedList,
                                         include_xy=T)
  seedListExact<-lapply(maskList,function(mskl){
    mskl[which.min(mskl$value),]
  })
  seedListExact<-do.call(rbind,seedListExact)
  maskListExact<-do.call(rbind,maskList)

  cellListExact<-raster::cellFromXY(fn_srt,maskListExact[,c('x','y')])

  newSrt<-fn_srt

  newSrt[cellListExact]<-0
browser()
  ######### here starts segmentation

  DFraster<-raster::as.data.frame(fn_srt,xy=T)
  colnames(DFraster)<-c('x','y','value')
  cellIndex<-raster::cellFromXY(fn_srt,DFraster[,c('x','y')])
  DFraster<-data.frame(cellIndex=cellIndex,DFraster,stringsAsFactors = F)
  DFraster<-DFraster[!is.na(DFraster$value),]
  DFraster<-DFraster[order(DFraster$value,decreasing = F),]

  bunchOfSeeds<-data.frame(seed= DFraster$cellIndex,
                           score=rep(0,nrow(DFraster)),
                           active=rep(1,nrow(DFraster)))
  rm(DFraster)
  lyr<-names(fn_srt)

  if (nrow(bunchOfSeeds)==0){
    if (!fn_returnRasters) fn_srt<-raster::raster(matrix(0))
    fakePolygon<-matrix(data = c(xmn,ymn,xmn+1,ymn,xmn+1,ymn+1,xmn,ymn+1,xmn,ymn),
                        ncol = 2,
                        byrow = T)
    colnames(fakePolygon)<-c('x','y')
    segmentationOut<-new('IMC_Segmentation',
                         polygons=list(fakePolygon),
                         performance=data.frame(Nseeds=numeric(0),
                                                Npoly=numeric(0),
                                                Ntime=numeric(0))
                         ,raster=fn_srt)
    return(segmentationOut)
  }

  polyList<-vector(mode = 'list',length = nrow(bunchOfSeeds))
  areaList<-vector(mode = 'list',length = nrow(bunchOfSeeds))

  rMsk<-RUNIMC:::radialMask(fn_radius,fn_Nspikes)
  sinMatrix<-rMsk$sinMatrix
  cosMatrix<-rMsk$cosMatrix
  intersectionMatrix<-rMsk$intersectionMatrix

  polyIndex=0
  cycleIndex=1
  oldCycleIndex=1
  oldPolyIndex=0
  oldSeedIndex=0
  seedPointer=1
  activeSeed<-c()
  switchOff<-sum(bunchOfSeeds$active)
  cycleWindowProportion<-fn_cycleWindow/1000

  while(switchOff>0){


    logicActive<-bunchOfSeeds$active==1
    minPointerScore<-min(bunchOfSeeds$score[logicActive])
    logicScore<-bunchOfSeeds$score==minPointerScore
    seedPointer<-which(logicActive & logicScore)[1]
    bunchOfSeeds$score[seedPointer]<-bunchOfSeeds$score[seedPointer]+1
    activeSeed<-bunchOfSeeds$seed[seedPointer]


    cat(paste0(lyr,' / ',
               'N of seeds: ',formatC(switchOff,flag='0',digits = 9),
               ' / N of polygons: ',formatC(polyIndex,flag='0',digits = 9),
               ' / cycleIndex: ', formatC(cycleIndex,flag='0',digits=9),
               # ' / discovery window |',paste0(rep("@",TEMP_windowFraction),collapse = ""),
               # paste0(rep("_",TEMP_remaining),collapse = ""),
               '| / throwout after: ',
               formatC(fn_seedOutScore,flag='0',digits=3),
               ' / Area range: ',
               formatC(fn_minArea,flag=' ',digits=3),
               ' % ',
               formatC(fn_maxArea,flag=' ',digits=3),
               ' / Roundness range: ',
               formatC(fn_minRoundness,flag=' ',digits=3),
               ' % ',
               formatC(fn_maxRoundness,flag=' ',digits=3),
               '\r'))

    TEMP<-raster::xyFromCell(fn_srt,activeSeed)

    xCoords<-TEMP[1]
    yCoords<-TEMP[2]

    xScan<-sinMatrix+xCoords
    yScan<-cosMatrix+yCoords

    xScan[xScan<xmn]<-NA
    yScan[yScan<ymn]<-NA
    xScan[xScan>xmx]<-NA
    yScan[yScan>ymx]<-NA
    xyScan<-matrix(1:length(xScan),ncol=ncol(xScan),nrow=nrow(xScan))

    xyVal<-apply(xyScan, c(1,2),function(x){
      ifelse((is.na(xScan[[x]]) | is.na(yScan[[x]])),
             NA,
             fn_srt[raster::cellFromXY(fn_srt,c(xScan[[x]],yScan[[x]]))]
      )
    })

    orgVal<-fn_srt[raster::cellFromXY(fn_srt,c(xCoords,yCoords))]


    switch(fn_segmentAlg,
           sign_simple = {
             spikeEnds<-apply(xyVal,1,.segmentBySign_simple,orgVal)
           },
           sign_extended = {
             spikeEnds<-apply(xyVal,1,.segmentBySign_extended,orgVal,fn_minArea,fn_maxArea)
           },
           quadratic = {
             bigX<-matrix(c(rep(1,fn_radius),(1:fn_radius)^2,1:fn_radius),byrow = F,ncol = 3)
             spikeEnds<-apply(xyVal,1,.segmentByQuadratic,orgVal,fn_favourForeing,bigX)
           })



    polyG<-try(t(sapply(1:fn_Nspikes,function(x){
      intersectionMatrix[x,spikeEnds[1,x]][[1]][spikeEnds[2,x]+1,]+c(xCoords,yCoords)
    })))

    if (inherits(polyG,'try-error'))
    colnames(polyG)<-c('x','y')

    polyG<-na.omit(polyG)
    notDup<-!duplicated(polyG)
    polyG<-polyG[notDup,,drop=F]

    if (!(nrow(polyG)<3)){
      polyG<-rbind(polyG,polyG[1,])
      polyArea<-RUNIMC:::.area(polyG)
      polyPerimeter<-RUNIMC:::.perimeter(polyG)
      polyRoundness<-RUNIMC:::.roundness(polyArea,polyPerimeter)

      if (fn_returnKinetic) TimingFunction<-rbind(TimingFunction,data.frame(Nseeds=nrow(bunchOfSeeds),Npoly=polyIndex,Ntime=Sys.time()))
      if (polyArea>=fn_minArea & polyArea<=fn_maxArea & polyRoundness>=fn_minRoundness & polyRoundness<=fn_maxRoundness){
        sfPolygon<-sf::st_polygon(list(as.matrix(polyG)),dim="XY")
        coverageDF <- (exactextractr::exact_extract(fn_srt,sf::st_sfc(sfPolygon),include_xy=T))[[1]]
        coverageDF<-coverageDF[coverageDF$coverage_fraction>=fn_coverage,]
        coordDF<-raster::cellFromXY(fn_srt,as.matrix(coverageDF[,c(2,3)]))
        bunchOfSeeds$active[bunchOfSeeds$seed %in% coordDF]<-0
        polyIndex=polyIndex+1
        fn_srt[coordDF]<-(-polyIndex)
        polyList[[polyIndex]]<-sfPolygon
        areaList[[polyIndex]]<-polyArea
      }
    }


    if ((cycleIndex-oldCycleIndex)>switchOff*cycleWindowProportion){

      if (polyIndex==oldPolyIndex & fn_adaptative==F) {break()}

      if (polyIndex==oldPolyIndex & fn_adaptative==T & fn_seedOutScore>1) {
        fn_seedOutScore=fn_seedOutScore-1
        fn_minArea<-fn_minArea-(fn_minArea*fn_areaAdaptRate)
        fn_maxArea<-fn_maxArea+(fn_maxArea*fn_areaAdaptRate)
        fn_minRoundness<-fn_minRoundness-(fn_minRoundness*fn_roundnessAdaptRate)
        fn_maxRoundness<-fn_maxRoundness+(fn_maxRoundness*fn_roundnessAdaptRate)
      }
      oldCycleIndex=cycleIndex
      oldPolyIndex=polyIndex
    }

    bunchOfSeeds$active[bunchOfSeeds$score>fn_seedOutScore]<-0
    switchOff<-sum(bunchOfSeeds$active)
    cycleIndex=cycleIndex+1
  }

  cat(paste0(lyr,' / ',
             'N of seeds: ',formatC(sum(bunchOfSeeds$active),flag='0',digits = 9),
             ' / N of polygons: ',formatC(polyIndex,flag='0',digits = 9),
             ' / cycleIndex: ', formatC(cycleIndex,flag='0',digits=9),
             # ' / discovery window |',paste0(rep("@",TEMP_windowFraction),collapse = ""),
             # paste0(rep("_",TEMP_remaining),collapse = ""),
             '| / throwout after: ',
             formatC(fn_seedOutScore,flag='0',digits=3),
             ' / Area range: ',
             formatC(fn_minArea,flag=' ',digits=3),
             ' % ',
             formatC(fn_maxArea,flag=' ',digits=3),
             ' / Roundness range: ',
             formatC(fn_minRoundness,flag=' ',digits=3),
             ' % ',
             formatC(fn_maxRoundness,flag=' ',digits=3),
             '\n'))

  polyList<-polyList[1:polyIndex]
  areaList<-areaList[1:polyIndex]


  if (fn_fusion){

    polyFSC<-sf::st_sfc(polyList)
    polyIntersection<-sf::st_intersects(polyFSC,sparse=F)

    if (is.character(fn_targetArea)){
      fn_targetArea
      switch(fn_targetArea,
             predicted_mean = {
               fn_targetArea<-mean(unlist(areaList))
             },
             predicted_median = {
               fn_targetArea<-median(unlist(areaList))
             },
             predicted_mode = {
               brks<-0:ceiling(unlist(areaList))
               frqT<-hist(x = unlist(areaList),breaks = brks)
               fn_targetArea<-median(frqT$breaks[which(frqT$counts==max(frqT$counts))])
             },
             predicted_max = {
               fn_targetArea<-max(unlist(areaList))
             },
             predicted_middle = {
               fn_targetArea<-(max(unlist(areaList))-min(unlist(areaList)))/2
             })
    }

    nt<-igraph::graph_from_adjacency_matrix(adjmatrix = polyIntersection,mode = 'undirected',diag = F)
    igraph::V(nt)$name<-as.character(1:nrow(polyIntersection))
    nt_edgeList<-igraph::as_edgelist(nt,names=T)
    nt_components<-igraph::components(nt)
    nt_subgraphIndexVector<-igraph::membership(nt_components)
    nt_subgraphIndices<-unique(nt_subgraphIndexVector)
    nt_subgraph<-lapply(nt_subgraphIndices,function(i){
      out<-igraph::induced_subgraph(nt,names(nt_subgraphIndexVector[nt_subgraphIndexVector==i]))
      return(out)
    })
    nt_length<-lapply(nt_subgraph,igraph::gsize)
    nt_maxLength<-max(unlist(nt_length))

    while(nt_maxLength>fn_maxNetworkSize){
      TEMP<-lapply(nt_subgraph,function(sbg){
        nt_sbgLength<-igraph::gsize(sbg)
        if (nt_sbgLength>fn_maxNetworkSize){
          newCommunity<-igraph::edge.betweenness.community(sbg)
          newMembers<-igraph::membership(newCommunity)
          newMembersIndex<-unique(newMembers)
          newSubgraph<-lapply(newMembersIndex,function(i){
            newCommunityVertex<-names(newMembers[newMembers==i])
            newSubSub<-igraph::induced_subgraph(nt,newCommunityVertex)
            return(newSubSub)
          })
        } else (newSubgraph<-sbg)
      })
      newListSubgraph<-list()
      newLSIndex<-1
      for (i in 1:length(TEMP)){
        if(class(TEMP[[i]])=='list'){
          for (ii in 1:length(TEMP[[i]])){
            newListSubgraph[[newLSIndex]]<-TEMP[[i]][[ii]]
            newLSIndex<-newLSIndex+1
          }} else{
            newListSubgraph[[newLSIndex]]<-TEMP[[i]]
            newLSIndex<-newLSIndex+1
          }
      }
      nt_subgraph<-newListSubgraph
      nt_length<-lapply(nt_subgraph,igraph::gsize)
      newMaxLength<-max(unlist(nt_length))
      if (newMaxLength==nt_maxLength) {nt_maxLength<-0} else {nt_maxLength<-newMaxLength}
    }

    newPolyList<-lapply(nt_subgraph,function(nts){

      nt_edges<-igraph::as_edgelist(nts)
      if (nrow(nt_edges)>0){

        nt_vertex<-unique(as.vector(nt_edges))
        nt_permutations<-t(expand.grid(rep(list(c(T,F)),nrow(nt_edges))))

        new_deltaPermutations<-pbapply::pbapply(nt_permutations,2,function(prmt){

          new_edges<-nt_edges[prmt,,drop=F]
          new_vertex<-nt_vertex[!(nt_vertex %in% unique(as.vector(new_edges)))]
          new_emptyEdges<-t(sapply(new_vertex,function(x){c(x,NA)},USE.NAMES = F,simplify = T))
          new_subgraph<-igraph::graph_from_edgelist(new_edges, directed = F)
          new_subgraph<-igraph::add_vertices(new_subgraph,length(new_vertex),name=new_vertex)
          new_components<-igraph::components(new_subgraph)
          new_groups<-sapply(unique(new_components$membership),function(x){
            polygonsInGroup<-names(new_components$membership[new_components$membership==x])
          },USE.NAMES = T,simplify = F)
          new_area<-lapply(new_groups,function(x){
            sum(unlist(areaList[as.numeric(x)]))
          })

          new_cumulativeDelta<-sum((unlist(new_area)-fn_targetArea)^2)
          return(list(area=new_cumulativeDelta,graph=new_subgraph))

        })

        mm<-lapply(new_deltaPermutations,function(x)x$area)
        mm<-which(mm==min(unlist(mm)))[1]
        winnerPermutation<-new_deltaPermutations[[mm]]$graph
        winnerComponents<-igraph::components(winnerPermutation)
        winnerPolygons<-sapply(unique(winnerComponents$membership),function(x){
          polygonsInGroup<-names(winnerComponents$membership[winnerComponents$membership==x])
          unionPolygon<-sf::st_union(sf::st_sfc(polyList[as.numeric(polygonsInGroup)]))
          unionPolygon<-sf::st_buffer(unionPolygon,fn_inflateDeflate)
          unionPolygon<-sf::st_buffer(unionPolygon,-fn_inflateDeflate)
          unionPolygon<-nngeo::st_remove_holes(unionPolygon)
          return(unionPolygon)})

        return (winnerPolygons)
      } else (return(polyList[[as.numeric(igraph::V(nts)$name)]]))

    })
    newPolyList<-RUNIMC:::.wiseUnlist(newPolyList)

    newPolyList<-lapply(newPolyList,function(x){
      if (class(x)=='list') {return(x[[1]])} else (return(x))
    })

  } else { newPolyList<-unlist(sf::st_sfc(polyList),recursive = F)}

  if (fn_returnKinetic) TimingFunction<-rbind(TimingFunction,data.frame(Nseeds=nrow(bunchOfSeeds),Npoly=polyIndex,Ntime=Sys.time()))
  if (!fn_returnRasters) fn_srt<-raster::raster(matrix(0))

  segmentationOut<-new('IMC_Segmentation',polygons=newPolyList,performance=TimingFunction,raster=fn_srt)
  return(segmentationOut)
}


#### prototype with sum bugs
# .segmentBySign<-function(profileVector,orgVal){
#   profileVector[profileVector<0]<-0
#   profileVector[is.na(profileVector)]<-0
#   shiftVector<-c(orgVal,profileVector[-length(profileVector)])
#   signVector<-sign(profileVector-shiftVector)
#   zeroSign<-which(signVector==0)
#   zeroSubstitute<-zeroSign-1
#   zeroSubstitute[zeroSubstitute==0]<-2
#   signVector[zeroSign]<-signVector[zeroSubstitute]
#   runLength<-rle(signVector)
#   runLength$lengths<-cumsum(runLength$lengths)
#   topS<-profileVector[runLength$lengths[runLength$values == 1]]
#   bottomS<-profileVector[runLength$lengths[runLength$values == -1]]
#   T_winner<-topS/c(orgVal,bottomS[(1:length(topS))-1])
#   winner<-which.max(T_winner)
#   winner<-runLength$lengths[runLength$values == 1][winner]
#   if (length(winner)==0) winner<-length(profileVector)
#   if (any(profileVector[1:winner]==0)){
#     winner<-which(profileVector[1:winner]==0)[1]
#     return(list=c(winner,0))
#   }
#   return(list=c(winner,1))
# }

#### chose peck with larger peck to valley ratio
.segmentBySign_simple<-function(profileVector,orgVal){

  profileVector[profileVector<0]<-0
  profileVector[is.na(profileVector)]<-0
  shiftVector<-c(orgVal,profileVector[-length(profileVector)])
  signVector<-sign(profileVector-shiftVector)
  signVector_1<-sign(profileVector-shiftVector)
  zeroSign<-which(signVector==0)

  if (length(zeroSign)>0){

    for (i in zeroSign){
      ifelse(i==1 & length(signVector)>=2,
             {signVector_1[i] = signVector[i+1]},
             {
               plus<-which(signVector[1:i]==1)
               if (length(plus)>1) plus<-max(plus)
               minus<-which(signVector[1:i]==-1)
               if (length(minus)>1) minus<-max(minus)
               if (length(plus)==0 & length(minus)==0) return(list=c(1,0))
               pm<-max(c(plus,minus))
               if (signVector[pm]==1) signVector_1[i]=1 else signVector_1[i]=-1

             })
    }
  }

  runLength<-rle(signVector_1)
  runLength$lengths<-cumsum(runLength$lengths)
  topS<-profileVector[runLength$lengths[runLength$values == 1]]
  bottomS<-profileVector[runLength$lengths[runLength$values == -1]]

  if (runLength$values[1]==1) dnmntr<- c(orgVal,bottomS[(1:length(topS))-1]) else dnmntr<-bottomS[(1:length(topS))]
  T_winner<-topS/dnmntr
  winner<-which.max(T_winner)
  winner<-runLength$lengths[runLength$values == 1][winner]
  if (length(winner)==0) winner<-length(profileVector)
  if (any(profileVector[1:winner]==0)){
    winner<-which(profileVector[1:winner]==0)[1]
    return(list=c(winner,0))
  }
  return(list=c(winner,1))
}


#### chose peak with largest peack to valley ratio and lower distance from
#### expected radius
.segmentBySign_extended<-function(profileVector,orgVal,minA,maxA){

  profileVector[profileVector<0]<-0
  profileVector[is.na(profileVector)]<-0
  shiftVector<-c(orgVal,profileVector[-length(profileVector)])
  signVector<-sign(profileVector-shiftVector)
  signVector_1<-sign(profileVector-shiftVector)
  zeroSign<-which(signVector==0)

  if (length(zeroSign)>0){

    for (i in zeroSign){
      ifelse(i==1 & length(signVector)>=2,
             {signVector_1[i] = signVector[i+1]},
             {
               plus<-which(signVector[1:i]==1)
               if (length(plus)>1) plus<-max(plus)
               minus<-which(signVector[1:i]==-1)
               if (length(minus)>1) minus<-max(minus)
               if (length(plus)==0 & length(minus)==0) return(list=c(1,0))
               pm<-max(c(plus,minus))
               if (signVector[pm]==1) signVector_1[i]=1 else signVector_1[i]=-1

             })
    }
  }

    runLength<-rle(signVector_1)
  runLength$lengths<-cumsum(runLength$lengths)
  topS<-profileVector[runLength$lengths[runLength$values == 1]]
  topS_area<-abs(((pi*topS^2)-((maxA-minA)/2)))

  bottomS<-profileVector[runLength$lengths[runLength$values == -1]]

  if (runLength$values[1]==1) dnmntr<- c(orgVal,bottomS[(1:length(topS))-1]) else dnmntr<-bottomS[(1:length(topS))]
  T_winner<-topS/dnmntr
  T_winner<-T_winner/topS_area
  winner<-which.max(T_winner)
  winner<-runLength$lengths[runLength$values == 1][winner]
  if (length(winner)==0) winner<-length(profileVector)
  if (any(profileVector[1:winner]==0)){
    winner<-which(profileVector[1:winner]==0)[1]
    return(list=c(winner,0))
  }
  return(list=c(winner,1))
}


###chose peack as maximum of a quadratic curve
.segmentByQuadratic<-function(profileVector,orgVal,fn_favourForeing,bigX){

  profileVector[profileVector<0]<-0
  profileVector[is.na(profileVector)]<-0
  if (fn_favourForeing & any(profileVector==0)){
    peakRound<-which(profileVector==0)[1]
    u<-0
    return(list=c(peakRound,u))
  } else {

    fitPoly<-.lm.fit(bigX,profileVector)
    a <- coef(fitPoly)[2]
    b <- coef(fitPoly)[3]
    peak<-(-(b/(2*a)))
    peakRound<-round(peak)
    hillSide<-peak-peakRound

    if (is.null(peakRound) | is.na(peakRound) | length(peakRound)==0){
      peakRound<-0
      hillside<-0
    }
    if (peakRound>length(profileVector)) peakRound<-length(profileVector)
    if (peakRound<1) {
      if (any(profileVector==0)){
        peakRound<-min(which(profileVector==0))[1]
        hillSide<-0
      } else {
        peakRound<-length(peakRound)
        hillSide<-1}
    }
    if (any(profileVector==0)){
      if (peakRound>min(which(profileVector==0))[1]){
        peakRound<-min(which(profileVector==0))[1]
        hillSide<-0
      }
    }
    if (hillSide<=0.5) u<-0 else u<-1
    return(list=c(peakRound,u))}
}
