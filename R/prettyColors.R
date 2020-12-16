prettyColors<-function(fn_nSamples,
                       fn_colorSet='Set1',
                       fn_nColors=9){
  csubs<-grDevices::colorRampPalette(RColorBrewer::brewer.pal(n=fn_nColors,name=fn_colorSet))(fn_nSamples)
  csubs<-csubs[order(csubs)]
  return(csubs)
}
