#'Study initialization
#'
#'initStudy provide functionality to initialize and crate the infrastructure to accommodate a new study
#' @param fn_studyName character, name of the study
#' @param fn_rootFolder character, directory where to store all the files inherent to this study
#' @param fn_rawDataFolder character, directory containing the raw data txt files
#' @param fn_whichFiles numeric vector, which files should be loaded (alphabetical order as produced by list.files). if NULL (default) all files will be loaded
#' @param fn_whichColumns character, can be either 'named' (default) or 'all'. Header of the txt table will be parsed in search of names of possible markers, providing 'named', only those columns that seem to contain a name are loaded
#' @param fn_transpose logical, should the raster be transposed?
#' @param fn_overwrite logical, if true and a study with the same name is present on the disk, files will be delete first. False, produce a milder effect producing a progressive overwrite
#' @param fn_verbose logical, describe what is going on?
#' @return An environment containing a new study and a hierarchy of files where specified
#' @examples
#' \dontrun{
#' study<-initStudy('TEST_study',
#' 'c:/Data/whereToStoreStudy,
#' 'c:/Data/whereToGetRawData,
#' NULL,
#' 'named',
#' F,
#' F,
#' T)
#' }
#' @export
initStudy<-function(fn_studyName='IMCstudy',
                    fn_rootFolder=NULL,
                    fn_rawDataFolder=NULL,
                    fn_whichFiles=NULL,
                    fn_whichColumns='named',
                    fn_transpose=F,
                    fn_overWrite=F,
                    fn_verbose=T,
                    ...){

  if (is.null(fn_studyName)) stop(mError('provide a name for the study'),call. = F)
  if (is.null(fn_rootFolder)) stop(mError('provide a folder where to store the study'),call. = F)
  if (is.null(fn_rawDataFolder)) stop(mError('provide a folder containing raw data'),call. = F)
  if (fn_overWrite){
    targetFolder<-file.path(fn_rootFolder,fn_studyName)
    if (dir.exists(targetFolder)) unlink(targetFolder,recursive = T,force = T)
  }

  studyFolder<-checkDir(fn_rootFolder,fn_studyName,verbose=fn_verbose)
  rasterFolder<-checkDir(studyFolder,'rasters',verbose=fn_verbose)
  stackFolder<-checkDir(studyFolder,'rasterStacks',verbose=fn_verbose)
  analysisFolder<-checkDir(studyFolder,'analysis',verbose=fn_verbose)

  rawDataFiles<-list.files(fn_rawDataFolder,full.names = T,pattern = '*.txt',recursive = F)
  if (!is.null(fn_whichFiles)) rawDataFiles<-rawDataFiles[fn_whichFiles]

  headersLine<-lapply(rawDataFiles,function(flnm){
    con <- file(flnm,"r")
    headerLine <- readLines(con,n=1)
    close(con)
    return(headerLine)
  })

  masterHeader<-unique(headersLine)[[1]]

  if (length(unique(headersLine))>1) {stop("Headers from different files do not match")}

  masterHeader<-strsplit(masterHeader,'\t',fixed=T)[[1]]
  if (fn_whichColumns=='all') ldld<-T

  Channels<-lapply(masterHeader,function(mstrhdr){

    if (!any(mstrhdr %in% c('X','x','Y','y','Z','z','Start_push','End_push','Pushes_duration'))){

      mstrhdrSplit<-strsplit(mstrhdr,'-')[[1]]
      if (length(mstrhdrSplit)==1){
        chnl<-gsub("\\(.+?\\)", "", mstrhdrSplit[1])
        mrkr<-''
        if (fn_whichColumns=='named') ldld<-F
      } else
        if (length(mstrhdrSplit)==2){
          chnl<-mstrhdrSplit[1]
          mrkr<-gsub("\\(.+?\\)", "", mstrhdrSplit[2])
          if (fn_whichColumns=='named') ldld<-T
        } else {stop('Something weird whith the raw files...')}
      chnls<-data.frame(columnNames=mstrhdr,
                        RcolumnNames=make.names(mstrhdr,unique = F,allow_ = T),
                        channel=chnl,
                        marker=mrkr,
                        loaded=ldld,
                        stringsAsFactors = F)
    }
  })

  Channels<-do.call(rbind.data.frame,Channels)
  Channels<-new('IMC_ChannelTable',Channels)

  ### load rasters

  rawDataFiles<-list.files(fn_rawDataFolder)

  rst<-lapply(rawDataFiles,
              function(tblFile){
                oneRasterFolder<-checkDir(rasterFolder,tblFile)
                rst<-loadTxtToStack(fn_path = fn_rawDataFolder,
                                    fn_file = tblFile,
                                    fn_rasterDBPath = oneRasterFolder,
                                    fn_rasterStackPath = stackFolder,
                                    fn_cols = Channels$columnNames[Channels$loaded],
                                    fn_newNames =  Channels$RcolumnNames[Channels$loaded],
                                    fn_details =  list(study=fn_studyName),
                                    fn_channel = Channels,
                                    fn_transpose = fn_transpose,
                                    fn_norm = F,
                                    fn_trsh = NULL,
                                    fn_zeroOff = NULL,
                                    ...)
                return(rst)})

  names(rst)<-lapply(rst,function(x){x@uid})

  rst<-new('IMC_RsCollection',rst)
  attr(rst,'artnTimeStmp')<-attr(rst,'mdtnTimeStmp')
  attr(rst,'fileArchive')<-stackFolder

  studyTable<-lapply(rst,function(x){
    data.frame(
      uid=x@uid,
      IMC_folder=fn_rawDataFolder,
      IMC_text_file=x@IMC_text_file,
      study=x@study,
      sample=x@sample,
      replicate=x@replicate,
      ROI=x@ROI,
      bioGroup=x@bioGroup,
      stringsAsFactors = F)})

  studyTable<-do.call(rbind.data.frame,studyTable)
  studyTable<-new('IMC_StudyTable',studyTable)

  # newStudy<-new('IMC_Study')
  newStudy<-new.env()
  newStudy<-initObjectAttr(newStudy)

  newStudy$name<-fn_studyName
  newStudy$rootFolder<-fn_rootFolder
  newStudy$rawDataFolder=fn_rawDataFolder
  newStudy$studyTable = studyTable
  newStudy$raster=rst
  newStudy$whichColumns=fn_whichColumns
  newStudy$channels=Channels
  newStudy$analysis=NULL
  newStudy$currentAnalysis=NULL

  return(newStudy)
}
