#' @import methods
#' @import raster
#' @import randomForest
#' @importFrom  MASS kde2d
#' @import RColorBrewer
#' @import XML
#' @import digest
#' @importFrom dplyr bind_cols
#' @importFrom exactextractr exact_extract
#' @importFrom imager as.cimg vanvliet deriche blur_anisotropic
#' @import lwgeom
#' @import scales
#' @import sf
#' @import shiny
#' @import sp
#' @import keys
#' @import shinydashboard
#' @importFrom graphics abline points title
#' @importFrom stats D coef median na.omit nls.control

mError<-crayon::red$inverse
mWarning<-crayon::yellow$inverse
mMessage<-crayon::green$inverse

