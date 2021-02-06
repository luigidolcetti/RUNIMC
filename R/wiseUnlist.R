.wiseUnlist<-function(fn_list){
  fn_list<-list(fn_list)
  envTEMP<-new.env()
  envTEMP$newList<-list()
  flg=T
  while (flg){
    fn_list<-lapply(fn_list,function(x){

      chk<-sapply(x,function(y){is.list(y) & length(y)!=0},simplify = T,USE.NAMES = F)
      for (ii in 1:length(chk)){
        if (!chk[[ii]]) {
          assign(x = 'newList',
                 value = append(envTEMP$newList,list(x[[ii]])),
                 envir = envTEMP)}
      }

      return (x[chk])
    })
    chk<-sapply(fn_list,is.list,simplify = T,USE.NAMES = F)
    if (all(!chk)) flg<-F
    fn_list<-unlist(fn_list,recursive = F)
  }
  out<-sapply(envTEMP$newList,function(x){length(x)!=0},simplify = T,USE.NAMES = F)
  return(envTEMP$newList[out])
}
