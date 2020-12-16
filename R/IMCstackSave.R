#'Some BullShit
#'
#'
#' @export
IMCstackSave<-function (x, filename)
{
  filename <- trim(filename)
  if (filename == "") {
    stop("Provide a non empty filename.")
  }
  info <- t(sapply(x@layers, function(i) c(i@file@name)))
  info<-c(info,'****info section****',x@uid,x@IMC_text_file,x@study,x@sample,x@replicate,x@ROI,x@bioGroup)
  utils::write.table(info, filename, row.names = FALSE,
                     col.names = FALSE)
  x@filename <- filename
  return(x)
}
