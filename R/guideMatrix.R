guideMatrix<-function(fn_rf){

  featureLabel<-fn_rf$classes
  core<-sapply(featureLabel,function(x){unlist(strsplit(x,"_"))[2]})
  coreLabel<-data.frame(core,stringsAsFactors = F)
  colnames(coreLabel)<-"level"
  nlabels<-nrow(coreLabel)
  coreLabel<-cbind(coreLabel,data.frame(label=1:nlabels,
                                        seed=rep(0,nlabels),
                                        include=rep(0,nlabels),
                                        action=rep(0,nlabels),
                                        stringsAsFactors = F))
  coreLabel<-rep(list(coreLabel),length(unique(core)))
  names(coreLabel)<-unique(core)
  return(coreLabel)
}
