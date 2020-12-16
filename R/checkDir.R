#'Some BullShit
#'
#'
checkDir<-function(parentalFolder,childFolder){
  folderList<-dir(parentalFolder)
  if (!(any (childFolder %in% folderList))){
    dir.create(paste0(parentalFolder,'/',childFolder))
    message(mMessage(paste0('\n',childFolder, ' folder created in ',parentalFolder)))
    return(paste(parentalFolder,childFolder,sep='/'))
  }
  message(mWarning(paste0('\n',childFolder, ' folder already exist in ',parentalFolder,', nothing to do')))
  return(paste(parentalFolder,childFolder,sep='/'))
}
