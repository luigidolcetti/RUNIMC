#'Some nice thing
#'
#'
#' @export
quantNorm<-function(fn_dt,
                    fn_trsh=0.997,
                    fn_zeroOff=T){
  xCoords<-ncol(fn_dt)
  yCoords<-nrow(fn_dt)
  if (fn_zeroOff){
    maxVal<- stats::quantile(fn_dt[fn_dt>0],probs = fn_trsh)} else {
      maxVal<- stats::quantile(fn_dt,probs = fn_trsh)
    }
  imgOut<-fn_dt/maxVal
  imgOut[imgOut>1]<-1
  return(imgOut)
}
