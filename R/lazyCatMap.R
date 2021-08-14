#'Some nice thing
#'
#'
#' @export
lazyCatMap<-function (fn_srt=NULL,
                      fn_uid=NULL,
                      fn_indexToExclude=NULL){


  if (is.null(fn_uid)) fn_uid<-NA
  if (!is.null(fn_indexToExclude)) {
    for (i in fn_indexToExclude){
      fn_srt[fn_srt==i]<-NA
    }
  }

  newStars<-stars::st_as_stars(fn_srt)

  newPoly<-sf::st_as_sf(newStars,merge=T)
  newPoly<-sf::st_buffer(newPoly,0)
  colnames(newPoly)[1]<-'ID'
  #### new part ####

  newCondensedPoly<-lapply(unique(newPoly$ID),function(x){
    out<-sf::st_union(newPoly[newPoly$ID==x,])
    out<-sf::st_sf(sf_column_name = 'geom',
                   geom = out,
                   uid = fn_uid,
                   ID = x,
                   stringsAsFactors = F)
  })

  newCondensedPoly<-do.call(dplyr::bind_rows,newCondensedPoly)
  return(newCondensedPoly)

}



