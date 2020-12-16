#'Some BullShit
#'
#'
#' @export
imageRFilter<-function(fn_rasterStack,
                       fn_filter,
                       fn_markerList,
                       fn_filterParameterList){

  defaults<-formals(eval(parse(text=paste0('imager::',fn_filter))))

  new_rasterStack<-sapply(names(fn_rasterStack),function(x)list(),USE.NAMES = T)

  for (st in names(fn_rasterStack)){

    xmn<-fn_rasterStack[[st]]@extent[1]
    xmx<-fn_rasterStack[[st]]@extent[2]
    ymn<-fn_rasterStack[[st]]@extent[3]
    ymx<-fn_rasterStack[[st]]@extent[4]


    for (i in fn_markerList){
      for (p in fn_filterParameterList){
        parametersList<-append(list(im=imager::as.cimg(fn_rasterStack[[st]][[i]])),p)
        parametersList<-append(parametersList,defaults[!(names(defaults) %in% names(parametersList))])
        newName<-paste(fn_filter,i,paste0(unlist(lapply(names(p),function(x){paste0(x,'_',p[x])}),recursive = F),collapse = '.'),sep='_')
        df<-do.call(eval(parse(text=paste0('imager::',fn_filter))),parametersList)
        df<-raster::raster(t(as.matrix(df)),xmn=xmn,xmx=xmx,ymn=ymn,ymx=ymx,crs=sp::CRS(as.character(NA)))
        new_rasterStack[[st]][[newName]]<-df
      }
    }
  }
  #
  new_rasterStack<-sapply(names(new_rasterStack),function(nms) IMC_stack(new_rasterStack[[nms]],
                                                                         uid=nms,
                                                                         IMC_text_file='memory',
                                                                         study='derivedRaster'),USE.NAMES = T)
  return(new_rasterStack)
}
