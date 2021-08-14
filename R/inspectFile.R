#' Inspect
#'
#' Inspect a raw data table for possible problems related to channel and markers
#'   names, coordinates, missing data.
#' @param fn_file character, path to a txt tab-delimited raw data table.
#' @return a character string that can be printed with **[cat()]**.
#' @export
inspectFile<-function(fn_file=NULL){
  if (is.null(fn_file)) stop(RUNIMC:::mError('need a file to inspect'),call. = F)
  if (!file.exists(fn_file)) stop(RUNIMC:::mError('cannot find this file'),call. = F)

  rawMatrix<-utils::read.table(fn_file,sep='\t',header = T,check.names = F,colClasses = 'numeric')

  numberOfColumns<-ncol(rawMatrix)
  if (numberOfColumns<3) return('number of columns is <3, check if table is tab (\\t) delimited!')

  numberOfRows<-nrow(rawMatrix)

  namesOfColumns<-colnames(rawMatrix)
  if (any(namesOfColumns=='X')) check.X <-crayon::bgGreen('OK') else check.X<-crayon::bgRed('ERROR')
  if (any(namesOfColumns=='Y')) check.Y <-crayon::bgGreen('OK') else check.Y<-crayon::bgRed('ERROR')

  emptyChannelPattern<-'[0-9]+[A-z]+\\([A-z]+[0-9]+[A-z]+\\)'
  usedChannelPattern<-'[0-9]+[A-z]+-[A-z0-9]+\\([A-z]+[0-9]+[A-z]+\\)'

  ECC<-grepl(emptyChannelPattern,namesOfColumns)
  UCC<-grepl(usedChannelPattern,namesOfColumns)
  OCC<-!ECC & !UCC

  if (check.X==crayon::bgGreen('OK') & check.Y==crayon::bgGreen('OK')){
    xCol <- length(unique(rawMatrix[,'X']))
    yCol <- length(unique(rawMatrix[,'Y']))
    xyLength <- xCol*yCol
    if (xyLength!=numberOfRows) check.matrix<-crayon::bgRed(crayon::bgRed('ERROR')) else check.matrix<-crayon::bgGreen('OK')
  }

  check.na.empty<-sapply(namesOfColumns[ECC],function(x){
    if (any(is.na(rawMatrix[,x]))) return (paste0(x,': ',crayon::bgRed(crayon::bgRed('ERROR')))) else return(paste0(x,': ',crayon::bgGreen(crayon::bgGreen('OK'))))
  },USE.NAMES = F,simplify = T)

  check.na.used<-sapply(namesOfColumns[UCC],function(x){
    if (any(is.na(rawMatrix[,x]))) return (paste0(x,': ',crayon::bgRed(crayon::bgRed('ERROR')))) else return(paste0(x,': ',crayon::bgGreen(crayon::bgGreen('OK'))))
  },USE.NAMES = F,simplify = T)

  uid = paste0('uid.',digest::digest(rawMatrix,seed=123))

  out<-paste(
    paste0('file:\t\t\t',fn_file),
    paste0('uid:\t\t\t',uid),
    paste0('X coords:\t\t',check.X),
    paste0('Y coords:\t\t',check.Y),
    paste0('columns:\t\t',numberOfColumns),
    paste0('rows:\t\t\t',numberOfRows),
    paste0('empty channels:\t\t',length(which(ECC)),'\n\n',paste(namesOfColumns[ECC],collapse = ', '),'\n'),
    paste0('used channels:\t\t',length(which(UCC)),'\n\n',paste(namesOfColumns[UCC],collapse = ', '),'\n'),
    paste0('other columns:\t\t',length(which(OCC)),'\n\n',paste(namesOfColumns[OCC],collapse = ', '),'\n'),
    paste0('data matrix:\t\t',check.matrix),
    paste0('na in empty:\n\t\t\t',paste(check.na.empty,collapse = '\n\t\t\t')),
    paste0('na in used:\n\t\t\t',paste(check.na.used,collapse = '\n\t\t\t')),
    sep = '\n')

  return(out)


}
