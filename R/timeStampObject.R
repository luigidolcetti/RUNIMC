#'Some BullShit
#'
#'
timeStampObject<-function(x,
                          crtn,
                          mdfn,
                          arcn){

  if (!missing(crtn)) attr(x,"crtnTimeStmp")<-crtn
  if (!missing(mdfn)) attr(x,"mdtnTimeStmp")<-mdfn
  if (!missing(arcn)) attr(x,"artnTimeStmp")<-arcn

  return(x)
}
