#'Some nice thing
#'
#'
checkDir<-function(parentalFolder,childFolder,verbose=T){
  folderList<-dir(parentalFolder)
  if (!(any (childFolder %in% folderList))){
    dir.create(paste0(parentalFolder,'/',childFolder))
    if (verbose) {message(mMessage(paste0('\n',childFolder, ' folder created in ',parentalFolder)))}
    return(paste(parentalFolder,childFolder,sep='/'))
  }
  if (verbose) message(mWarning(paste0('\n',childFolder, ' folder already exist in ',parentalFolder,', nothing to do')))
  return(paste(parentalFolder,childFolder,sep='/'))
}
