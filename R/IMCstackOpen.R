#'Some nice thing
#'
#'
#' @export
IMCstackOpen<-function (fn_stackFile)
{
  tblStk <- utils::read.table(fn_stackFile, as.is = FALSE, strip.white = TRUE)
  tblStk<-as.vector(tblStk[,1])
  startInfo<-which(tblStk=='****info section****')
  rstStk <- stack(as.vector(tblStk[1:(startInfo-1)]))
  rstStk<-as(rstStk,'IMC_RasterStack')
  rstStk@uid<-tblStk[startInfo+1]
  rstStk@IMC_text_file<-tblStk[startInfo+2]
  rstStk@study<-tblStk[startInfo+3]
  rstStk@sample<-tblStk[startInfo+4]
  rstStk@replicate<-tblStk[startInfo+5]
  rstStk@ROI<-tblStk[startInfo+6]
  rstStk@bioGroup<-tblStk[startInfo+7]
  rstStk@filename <- fn_stackFile
  return(rstStk)
}
