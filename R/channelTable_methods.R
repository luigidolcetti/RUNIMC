#' Channel Table content
#'
#'
#' Retrive or update content of the channel table. Some content such as R-names
#' are read only to prevent corruption of study cross references.
#'
#' @return an object of class IMC_ChannelTable or a character vector (named if different from R-columns)
#' @seealso
#' @examples
#' \dontrun{
#' default_study<-channelTable(x = MyStudy, loaded = NA)
#' default_study<-channelTable(x = MyStudy, loaded = T)
#' ch_channel(x = myStudy)<-('MySample1','MySample2','MySample3')
#' }
#' @export
#' @docType methods
#' @rdname ChannelTable-methods
setGeneric("channelTable", function(x,...)
  standardGeneric("channelTable"))

#' @details ChannelTable: retrive the entire study table, there is no assignment method
#' @docType methods
#' @aliases ChannelTable,environment-method
#' @rdname ChannelTable-methods
setMethod('channelTable',signature = ('environment'),
          function(x,...){
            return(x$channels)})

#####ch_Rnames####

#' @details ch_Rnames: retrive R compatible names, loaded (T), unloaded (F), all (NA)
#' @export
#' @docType methods
#' @rdname ChannelTable-methods
setGeneric("ch_Rnames", function(x,loaded=T, ...)
  standardGeneric("ch_Rnames"))

#' @docType methods
#' @aliases ch_Rnames,environment
#' @rdname ChannelTable-methods
setMethod('ch_Rnames',signature = ('environment'),
          function(x,loaded=T,...){
            if (is.na(loaded)) {
              out<-x$channels$RcolumnNames} else {
                out<-x$channels$RcolumnNames[x$channels$loaded==loaded]
              }
            return(out)
          })

#####ch_Onames####

#' @details ch_Onames: retrive the original names in raw data TXT file, loaded (T), unloaded (F), all (NA)
#' @export
#' @docType methods
#' @rdname ChannelTable-methods
setGeneric("ch_Onames", function(x,loaded=T, ...)
  standardGeneric("ch_Onames"))

#' @docType methods
#' @aliases ch_Onames,environment
#' @rdname ChannelTable-methods
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

#' @details ch_markers: retrive the original names in raw data TXT file, loaded (T), unloaded (F), all (NA)
#' @export
#' @docType methods
#' @rdname ChannelTable-methods
setGeneric("ch_markers", function(x,loaded=T, ...)
  standardGeneric("ch_markers"))

#' @docType methods
#' @aliases ch_markers,environment
#' @rdname ChannelTable-methods
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
#' @details ch_RnameFromMarker: retrive the R compatible name (usually use as layer name for raster objects) from the marker name
#' @export
#' @docType methods
#' @rdname ChannelTable-methods
setGeneric("ch_RnameFromMarker", function(x, marker,...)
  standardGeneric("ch_RnameFromMarker"))

#' @docType methods
#' @aliases ch_RnameFromMarker,environment
#' @rdname ChannelTable-methods
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

