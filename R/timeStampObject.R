#'Some BullShit
#'
#'
timeStampObject<-function(x,
                          crtn,
                          mdtn,
                          artn){

  if (!missing(crtn)) attr(x,"crtnTimeStmp")<-crtn
  if (!missing(mdtn)) attr(x,"mdtnTimeStmp")<-mdtn
  if (!missing(artn)) attr(x,"artnTimeStmp")<-artn

  return(x)
}
