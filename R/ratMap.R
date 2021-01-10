#'Some BullShit
#'
#'
#' @export
ratMap<-function (fn_srt,
                  fn_radius=10,
                  fn_Nspikes=4,
                  fn_minArea=10,
                  fn_maxArea=100,
                  fn_minRoundness=0.6,
                  fn_maxRoundness=0.8,
                  fn_coverage=0.3,
                  fn_seedOutScore=10,
                  fn_cycleWindow=1000,
                  fn_discoverTreshold=10e-3,
                  fn_adaptative=T,
                  fn_drastic=0,
                  fn_lowPenalty=2,
                  fn_highPenalty=1,
                  fn_roundnessPenalty=1,
                  fn_seed=1234){

  TimingFunction<-data.frame(Nseeds=0,Npoly=0,Ntime=Sys.time())

  xmn<-fn_srt@extent[1]
  xmx<-fn_srt@extent[2]
  ymn<-fn_srt@extent[3]
  ymx<-fn_srt@extent[4]

  # browser()
  DFraster<-raster::as.data.frame(fn_srt,xy=T)
  colnames(DFraster)<-c('x','y','value')
  cellIndex<-raster::cellFromXY(fn_srt,DFraster[,c('x','y')])
  DFraster<-data.frame(cellIndex=cellIndex,DFraster,stringsAsFactors = F)
  DFraster<-DFraster[!is.na(DFraster$value),]
  DFraster<-DFraster[order(DFraster$value,decreasing = F),]

  bunchOfSeeds<-data.frame(seed= DFraster$cellIndex,
                           score=0,
                           active=1)
  rm(DFraster)
  lyr<-names(fn_srt)

  if (nrow(bunchOfSeeds)==0){
    return(list(polygons=list(),
                performance=data.frame(Nseeds=numeric(0),Npoly=numeric(0),Ntime=numeric(0)),
                raster=fn_srt))}



  polyIndex=1
  polyList<-list()

  rMsk<-RUNIMC:::radialMask(fn_radius,fn_Nspikes)
  sinMatrix<-rMsk$sinMatrix
  cosMatrix<-rMsk$cosMatrix
  intersectionMatrix<-rMsk$intersectionMatrix

  cycleIndex=1
  oldCycleIndex=1
  oldPolyIndex=1
  oldSeedIndex=0
  seedPointer=1
  activeSeed<-c()
  #
  while(sum(bunchOfSeeds$active)>0){
    # browser()

    #
    if (length(activeSeed)==0){
      repeat{
        if (bunchOfSeeds$active[seedPointer]==1){

          activeSeed<-bunchOfSeeds$seed[seedPointer]
          bunchOfSeeds$score[seedPointer]<-bunchOfSeeds$score[seedPointer]+1
          if (seedPointer==nrow(bunchOfSeeds)) {seedPointer<-1} else {seedPointer<-seedPointer+1}
          break
        } else {

          if (seedPointer==nrow(bunchOfSeeds)) {seedPointer<-1} else {seedPointer<-seedPointer+1}
        }
      }
    }

    TEMP_windowFraction<-round((cycleIndex-oldCycleIndex)/fn_cycleWindow*10)
    TEMP_remaining<-10-TEMP_windowFraction

    cat(paste0(lyr,' / ',
               'N of seeds: ',formatC(sum(bunchOfSeeds$active),flag='0',digits = 9),
               ' / N of polygons: ',formatC(polyIndex,flag='0',digits = 9),
               ' / cycleIndex: ', formatC(cycleIndex,flag='0',digits=9),
               ' / discovery window |',paste0(rep("@",TEMP_windowFraction),collapse = ""),
               paste0(rep("_",TEMP_remaining),collapse = ""),
               '| / throwout after: ',
               formatC(fn_seedOutScore,flag='0',digits=3),
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
    # orgVal<-DFraster$value[DFraster$x==xCoords & DFraster$y==yCoords]

    spikeEnds<-apply(xyVal,1,
                     function(profileVector){
                       profileVector[profileVector<0]<-NA
                       gradienVector<-diff(c(orgVal,profileVector))
                       signVector<-sign(gradienVector)
                       signVector[is.na(signVector)]<-(-0.5)
                       scoreVector<-cumsum(signVector)
                       if (any(is.na(profileVector))){
                         i<-min(which(is.na(profileVector)))
                       } else {
                         i<-min(which(scoreVector==max(scoreVector)))
                       }

                       if (is.na(profileVector[i])) u<-0 else u<-1

                       return(list=c(i,u))
                     })


    polyG<-t(sapply(1:fn_Nspikes,function(x){
      intersectionMatrix[x,spikeEnds[1,x]][[1]][spikeEnds[2,x]+1,]+c(xCoords,yCoords)}))
    colnames(polyG)<-c('x','y')

    polyG<-na.omit(polyG)
    notDup<-!duplicated(polyG)
    polyG<-polyG[notDup,,drop=F]
    polyGXmax<-max(polyG[,'x'])
    polyGXmin<-min(polyG[,'x'])
    polyGYmax<-max(polyG[,'y'])
    polyGYmin<-min(polyG[,'y'])
    roughPolyArea<-(polyGXmax-polyGXmin)*(polyGYmax-polyGYmin)*pi/4

    if (roughPolyArea<fn_drastic) {
      polyGX<-seq(floor(polyGXmin),floor(polyGXmax))
      polyGY<-seq(floor(polyGYmin),floor(polyGYmax))
      polyGXY<-expand.grid(x=polyGX,y=polyGY)
      polyGCell<-apply(polyGXY,1,function(x) raster::cellFromXY(fn_srt,x))
      bunchOfSeeds$active[(bunchOfSeeds$seed %in% polyGCell)]<-0
      activeSeed<-c()
    } else {

      if (!(nrow(polyG)<3)){
        polyG<-rbind(polyG,polyG[1,])
        sfPolygon<-sf::st_polygon(list(as.matrix(polyG)),dim="XY")
        polyArea<-sf::st_area(sf::st_sfc(sfPolygon))
        polyPerimeter<-lwgeom::st_perimeter(sf::st_sfc(sfPolygon))
        polyRoundness<-4*pi*polyArea/(polyPerimeter^2)
        coverageDF <- (exactextractr::exact_extract(fn_srt,sf::st_sfc(sfPolygon),include_xy=T))[[1]]
        coverageDF<-coverageDF[coverageDF$coverage_fraction>=fn_coverage,]
        coordDF<-raster::cellFromXY(fn_srt,as.matrix(coverageDF[,c(2,3)]))

        TimingFunction<-rbind(TimingFunction,data.frame(Nseeds=nrow(bunchOfSeeds),Npoly=polyIndex,Ntime=Sys.time()))

        if (polyArea<fn_minArea) {
          for (icDf in coordDF){
            bunchOfSeeds$score[(bunchOfSeeds$seed==icDf)]<-bunchOfSeeds$score[(bunchOfSeeds$seed==icDf)]+fn_lowPenalty
          }
          activeSeed<-c()}
        else {
          if (polyArea>fn_maxArea){
            # activeSeed<-bunchOfSeeds[bunchOfSeeds$seed %in% coordDF,]
            # activeSeed<-activeSeed[sample(length(activeSeed),1)]
            # activeSeed<-activeSeed[1]
            # bunchOfSeeds$score[bunchOfSeeds$seed==activeSeed]<-bunchOfSeeds$score[bunchOfSeeds$seed==activeSeed]+fn_highPenalty
            bunchOfSeeds$score[seedPointer]<-bunchOfSeeds$score[seedPointer]+fn_highPenalty
            activeSeed<-c()
          } else {
            if (polyRoundness>=fn_minRoundness & polyRoundness<=fn_maxRoundness){
              bunchOfSeeds$active[bunchOfSeeds$seed %in% coordDF]<-0
              fn_srt[coordDF]<-(-polyIndex)
              polyList[[polyIndex]]<-polyG
              polyIndex=polyIndex+1
              activeSeed<-c()
            } else {
              bunchOfSeeds$score[seedPointer]<-bunchOfSeeds$score[seedPointer]+fn_roundnessPenalty
              activeSeed<-c()}
          }
        }
      } else {
        activeSeed<-c()}
    }

    if ((cycleIndex-oldCycleIndex)==fn_cycleWindow){
      DPI<-(polyIndex-oldPolyIndex)/(cycleIndex-oldCycleIndex)
      if (DPI<fn_discoverTreshold & fn_adaptative==F) {break()}
      if (DPI<fn_discoverTreshold &
          fn_adaptative==T &
          fn_seedOutScore>1) {fn_seedOutScore=fn_seedOutScore-1}
      oldCycleIndex=cycleIndex
      oldPolyIndex=polyIndex
    }
    bunchOfSeeds$active[bunchOfSeeds$score>fn_seedOutScore]<-0
    cycleIndex=cycleIndex+1
  }
  cat(paste0(lyr,' / ',
             'N of seeds: ',formatC(sum(bunchOfSeeds$active),flag='0',digits = 9),
             ' / N of polygons: ',formatC(polyIndex,flag='0',digits = 9),
             ' / cycleIndex: ', formatC(cycleIndex,flag='0',digits=9),
             ' / discovery window |',paste0(rep("@",TEMP_windowFraction),collapse = ""),
             paste0(rep("_",TEMP_remaining),collapse = ""),
             '| / throwout after: ',
             formatC(fn_seedOutScore,flag='0',digits=3),
             '\n'))

  TimingFunction<-rbind(TimingFunction,data.frame(Nseeds=nrow(bunchOfSeeds),Npoly=polyIndex,Ntime=Sys.time()))

  segmentationOut<-new('IMC_Segmentation',polygons=polyList,performance=TimingFunction,raster=fn_srt)
  return(segmentationOut)
}


