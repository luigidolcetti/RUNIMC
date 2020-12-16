#'Some BullShit
#'
#'
#' @export
initStudy<-function(fn_studyName='IMCstudy',
                    fn_rootFolder=NULL,
                    fn_rawDataFolder=NULL,
                    fn_whichColumns='named',
                    fn_overWrite=F,
                    ...){

  studyFolder<-checkDir(fn_rootFolder,fn_studyName)
  rasterFolder<-checkDir(studyFolder,'rasters')
  stackFolder<-checkDir(studyFolder,'rasterStacks')
  analysisFolder<-checkDir(studyFolder,'analysis')

  rawDataFiles<-list.files(fn_rawDataFolder,full.names = T)

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
