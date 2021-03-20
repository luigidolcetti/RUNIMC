#'Some BullShit
#'
#'
#' @export
alligatorMap<-function (fn_srt,
                        fn_radius=10,
                        fn_Nspikes=4,
                        fn_minArea=10,
                        fn_maxArea=100,
                        fn_minRoundness=0.6,
                        fn_maxRoundness=0.8,
                        fn_coverage=0.3,
                        fn_seedOutScore=10,
                        fn_cycleWindow=1,
                        fn_adaptative=T,
                        fn_areaAdaptRate=0,
                        fn_roundnessAdaptRate=0,
                        fn_segmentAlg = c('sign','quadratic'),
                        fn_fusion=T,
                        fn_targetArea=50,
                        fn_maxNetworkSize=4,
                        fn_inflateDeflate=0.1,
                        fn_favourForeing=T,
                        fn_returnKinetic = F,
                        fn_returnRasters = F){


  if (is.character(fn_targetArea)){
    if (!any(fn_targetArea == c('predicted_mean','predicted_median','predicted_mode','predicted_max','predicted_middle'))) stop(RUNIMC:::mError(('fn_targetArea should be a single numeric or "predicted_mean", "predicted_median, "predicted_mode", "predicted_max", "predicted_middle"')))
  }

  fn_segmentAlg<-match.arg(fn_segmentAlg)

  TimingFunction<-data.frame(Nseeds=0,Npoly=0,Ntime=Sys.time())

  xmn<-fn_srt@extent[1]
  xmx<-fn_srt@extent[2]
  ymn<-fn_srt@extent[3]
  ymx<-fn_srt@extent[4]

  #
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
           sign = {
             spikeEnds<-apply(xyVal,1,.segmentBySign,orgVal)
           },
           quadratic = {
             bigX<-matrix(c(rep(1,fn_radius),(1:fn_radius)^2,1:fn_radius),byrow = F,ncol = 3)
             spikeEnds<-apply(xyVal,1,.segmentByQuadratic,orgVal,fn_favourForeing,bigX)
           })



    polyG<-t(sapply(1:fn_Nspikes,function(x){
      intersectionMatrix[x,spikeEnds[1,x]][[1]][spikeEnds[2,x]+1,]+c(xCoords,yCoords)
    }))

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



.segmentBySign<-function(profileVector,orgVal){
  profileVector[profileVector<0]<-0
  profileVector[is.na(profileVector)]<-0
  shiftVector<-c(orgVal,profileVector[-length(profileVector)])
  signVector<-sign(profileVector-shiftVector)
  zeroSign<-which(signVector==0)
  zeroSubstitute<-zeroSign-1
  zeroSubstitute[zeroSubstitute==0]<-2
  signVector[zeroSign]<-signVector[zeroSubstitute]
  runLength<-rle(signVector)
  runLength$lengths<-cumsum(runLength$lengths)
  topS<-profileVector[runLength$lengths[runLength$values == 1]]
  bottomS<-profileVector[runLength$lengths[runLength$values == -1]]
  T_winner<-topS/c(orgVal,bottomS[(1:length(topS))-1])
  winner<-which.max(T_winner)
  winner<-runLength$lengths[runLength$values == 1][winner]
  if (length(winner)==0) winner<-length(profileVector)
  if (any(profileVector[1:winner]==0)){
    winner<-which(profileVector[1:winner]==0)[1]
    return(list=c(winner,0))
  }
  return(list=c(winner,1))
}

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
