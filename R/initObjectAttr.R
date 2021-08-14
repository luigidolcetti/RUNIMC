#'Some nice thing
#'
#'
initObjectAttr<-function(x){
  crtnTimeStmp<-format(Sys.time(),format="%F %T %Z", tz = Sys.timezone())
  attr(x,'crtnTimeStmp')<-crtnTimeStmp
  attr(x,'mdtnTimeStmp')<-crtnTimeStmp
  attr(x,'artnTimeStmp')<-NA
  attr(x,'fileArchive')<-NA
  return(x)
}

mdfObjectAttr<-function(x,
                              crtnTimeStmp,
                              mdtnTimeStmp,
                              artnTimeStmp,
                              fileArchive){
  attr(x,'crtnTimeStmp')<-crtnTimeStmp
  attr(x,'mdtnTimeStmp')<-mdtnTimeStmp
  attr(x,'artnTimeStmp')<-artnTimeStmp
  attr(x,'fileArchive')<-fileArchive
  return(x)
}
