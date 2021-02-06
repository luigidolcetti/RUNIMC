#'@export
setGeneric("listRasters", function(x, where=c('all','data','supplement','classification'), ...)
  standardGeneric("listRasters"))


setMethod('listRasters',signature = ('environment'),
          function(x,where = c('all','data','supplement','classification'), ...){
            where <- match.arg(where)
            uid<-st_uids(x)
            if (!is.null(x$raster)) rstList_data<-tableConstructor(x = x$raster,uid = uid,root='data',studyName=x$name) else rstList_data <- NULL
            # if (!is.null(x$currentAnalysis$derivedRasters)){
            #   rstList_supplement<-tableConstructor(x = x$currentAnalysis$derivedRasters,uid = uid,root='supplement')} else rstList_supplement <- NULL
            if (!is.null(x$currentAnalysis$classification)){
              rstList_classification<-tableConstructor(x = x$currentAnalysis$classification,uid = uid,root='classification',studyName=x$name)} else rstList_classification <- NULL

            rstList<-rbind.data.frame(rstList_data,
                                      # rstList_supplement,
                                      rstList_classification
                                      )

            rstList_data.tree<-data.tree::as.Node(rstList,
                                             mode = ('table'),
                                             pathName = 'path',
                                             pathDelimeter = '/')
            return(rstList_data.tree)
          })



tableConstructor<-function(x,uid,root,studyName){
  rstList_data<-sapply(uid,function(uid){

    extnt<-lapply(names(x[[uid]]), function(lyr) {
      raster::extent(x[[uid]][[lyr]])
    })
    xmn<-sapply(extnt,function(x) x[1],simplify = T,USE.NAMES = F)
    xmx<-sapply(extnt,function(x) x[2],simplify = T,USE.NAMES = F)
    ymn<-sapply(extnt,function(x) x[3],simplify = T,USE.NAMES = F)
    ymx<-sapply(extnt,function(x) x[4],simplify = T,USE.NAMES = F)

    out<-data.frame(where = root,
                    uid = uid,
                    layer = names(x[[uid]]),
                    type = sapply(names(x[[uid]]), function(lyr) {
                      if (is.null(raster::levels(x[[uid]][[lyr]]))) 'numeric' else 'categorical'
                    }),
                    xmn = xmn,
                    xmx = xmx,
                    ymn = ymn,
                    ymx = ymx,
                    min = sapply(names(x[[uid]]), function(lyr) {
                      raster::minValue(x[[uid]][[lyr]])
                    }),
                    max = sapply(names(x[[uid]]), function(lyr) {
                      raster::maxValue(x[[uid]][[lyr]])
                    }),
                    row.names = NULL,
                    stringsAsFactors = F)
    return(out)
  },USE.NAMES = F,simplify = F)

  rstList_data<-do.call(rbind.data.frame,rstList_data)
  pathString<-apply(rstList_data,1,function(x) paste(studyName,paste(x[c(2,1,3)],collapse='/'),sep='/'))
  rstList_data<-cbind.data.frame(data.frame(path = pathString,ID=seq_len(nrow(rstList_data)),stringsAsFactors = F),rstList_data[,4:10])
}
