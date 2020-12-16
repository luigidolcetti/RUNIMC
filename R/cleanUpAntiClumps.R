#'Some BullShit
#'
#'
#' @export
cleanUpAntiClumps<-function(fn_rstStack,
                            fn_Ncount=2,
                            fn_directions=4){


  lyrs<-names(fn_rstStack)

  out_rstStack<-lapply(lyrs,function(x){

    lvls<-levels(fn_rstStack[[x]])[[1]]
    lvlsNew<-max(lvls[,1])+1
    lvlsDF<-data.frame(ID=c(lvls$ID,lvlsNew),
                       label=c(as.character(lvls$label),'isolated'),
                       stringsAsFactors = T)

    newRst<-lapply(lvls[,1],function(y){

      rstLvels<-raster::deratify(fn_rstStack[[x]],att=1,drop=T,complete=T)
      rstLvels[rstLvels!=y]<-0
      clmps<-raster::clump(rstLvels,directions=fn_directions,gaps=F)

      clmpsTable<-freq(clmps)
      clmpsOut<-clmpsTable[clmpsTable[,'count']<fn_Ncount,'value']
      rstLvels[as.vector(clmps) %in% clmpsOut]<-lvlsNew
      return(rstLvels)
    })
    #
    newRst<-raster::stack(newRst)
    newRst<-raster::calc(newRst,sum)
    attr(newRst,'names')<-attr(fn_rstStack[[x]],'names')
    newRst<-raster::ratify(newRst)
    levels(newRst)<-lvlsDF
    return(newRst)
  })
  names(out_rstStack)<-names(fn_rstStack)
  return(out_rstStack)
}
