#' Channel Table content
#'
#' Retrive or update content of the channel table. Some content such as R-names
#'   are read only to prevent corruption of study cross references.
#'
#' @param x environment, a study.
#' @param ... not implemented.
#' @return an object of class IMC_ChannelTable.
#' @seealso
#' @examples
#' \dontrun{
#' default_study<-channelTable(x = MyStudy)
#' }
#' @export
setGeneric("channelTable", function(x,
                                    ...)
  standardGeneric("channelTable"))

setMethod('channelTable',signature = ('environment'),
          function(x,...){
            return(x$channels)})

#####ch_Rnames####

#' Channel Table content
#'
#' retrive R compatible channel name. These names are used as layer names
#'   for the raw data rasters.
#'
#' @param x environment, a study.
#' @param loaded logical, which channel? loaded(T), unloaded(F), all(NA).
#' @param ... not implemented.
#' @return character vector of names.
#' @examples
#' \dontrun{
#' default_study<-ch_Rnames(x = MyStudy, loaded = T)
#' }
#' @export
setGeneric("ch_Rnames", function(x,
                                 loaded=T,
                                 ...)
  standardGeneric("ch_Rnames"))

setMethod('ch_Rnames',signature = ('environment'),
          function(x,loaded=T,...){
            if (is.na(loaded)) {
              out<-x$channels$RcolumnNames} else {
                out<-x$channels$RcolumnNames[x$channels$loaded==loaded]
              }
            return(out)
          })

#####ch_Onames####

#' Channel Table content
#'
#' retrive original channel names as they appear as column names in the original
#'   raw data .txt file.
#'
#' @param x environment, a study.
#' @param loaded logical, which channel? loaded(T), unloaded(F), all(NA).
#' @param ... not implemented.
#' @return named character vector of names.
#' @examples
#' \dontrun{
#' default_study<-ch_Onames(x = MyStudy, loaded = T)
#' }
#' @export
setGeneric("ch_Onames", function(x,
                                 loaded=T,
                                 ...)
  standardGeneric("ch_Onames"))

setMethod('ch_Onames',signature = ('environment'),
          function(x,loaded=T,...){
            if (is.na(loaded)) {
              outNames<-x$channels$RcolumnNames
              out<-x$channels$columnNames
              names(out)<-outNames
            } else {
              outNames<-x$channels$RcolumnNames[x$channels$loaded==loaded]
              out<-x$channels$columnNames[x$channels$loaded==loaded]
              names(out)<-outNames
            }
            return(out)
          })

#####ch_markers####

#' Channel Table content
#'
#' retrieve the parsed name of the markers.
#'
#' @param x environment, a study.
#' @param loaded logical, which channel? loaded(T), unloaded(F), all(NA).
#' @param ... not implemented.
#' @return named character vector of names.
#' @examples
#' \dontrun{
#' default_study<-ch_markers(x = MyStudy, loaded = T)
#' }
#' @export
setGeneric("ch_markers", function(x,
                                  loaded=T,
                                  ...)
  standardGeneric("ch_markers"))

setMethod('ch_markers',signature = ('environment'),
          function(x,loaded=T,...){
            if (is.na(loaded)) {
              outNames<-x$channels$RcolumnNames
              out<-x$channels$marker
              names(out)<-outNames
            } else {
              outNames<-x$channels$RcolumnNames[x$channels$loaded==loaded]
              out<-x$channels$marker[x$channels$loaded==loaded]
              names(out)<-outNames
            }
            return(out)
          })


#####ch_RnameFromMarker####

#' Channel Table content
#'
#' retrieve the R compatible name of the channel corresponding to the specified
#'   marker.
#'
#' @param x environment, a study.
#' @param marker character, marker name.
#' @param ... not implemented.
#' @return named character.
#' @examples
#' \dontrun{
#' default_study<-ch_RnameFromMarker(x = MyStudy, marker = 'CD4')
#' }
#' @export
setGeneric("ch_RnameFromMarker", function(x,
                                          marker,
                                          ...)
  standardGeneric("ch_RnameFromMarker"))

setMethod('ch_RnameFromMarker',signature = ('environment'),
          function(x, marker,...){
            chnls<-x$channels$RcolumnNames[grep(marker,
                                                x$channels$marker,
                                                ignore.case = T)]
            names(chnls)<-x$channels$marker[grep(marker,
                                                 x$channels$marker,
                                                 ignore.case = T)]
            return(chnls)
          })

