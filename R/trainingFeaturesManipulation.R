.areaQuantile<-function(fn_trainingFeatures,
                        fn_quantiles){
  aggregate(area~label,fn_trainingFeatures,quantile,c(fn_quantiles))
}

if (!isGeneric("tf_areaQuantile")) {
  setGeneric("tf_areaQuantile", function(x,quantiles,...)
    standardGeneric("tf_areaQuantile"))
}

#'
#'
#' @export
setMethod('tf_areaQuantile',signature(x='IMC_TrainingFeatures'),
          function(x,quantiles){
            .areaQuantile(x$geometry,quantiles)
          })
#'
#'
#' @export
setMethod('tf_areaQuantile',signature(x='environment'),
          function(x,quantiles){

            if (!is.null(x$currentAnalysis$trainingFeatures)){
              out<-.areaQuantile(x$currentAnalysis$trainingFeatures$geometry,quantiles)
              return(out)

            } else {

              if (!is.null(x$trainingFeatures)){
                out<-.areaQuantile(x$trainingFeatures$geometry,quantiles)
                return(out)} else {
                  stop(RUNIMC:::mError("couldn't find any training featurs"))
                }
            }
          })

#####-------------------------------------------------------

.areaStatistics<-function(fn_trainingFeatures){
  aggregate(area~label,fn_trainingFeatures,summary)

}

if (!isGeneric("tf_areaStatistics")) {
  setGeneric("tf_areaStatistics", function(x,...)
    standardGeneric("tf_areaStatistics"))
}

#'
#'
#' @export
setMethod('tf_areaStatistics',signature(x='IMC_TrainingFeatures'),
          function(x){
            .areaStatistics(x$geometry)
          })
#'
#'
#' @export
setMethod('tf_areaStatistics',signature(x='environment'),
          function(x){

            if (!is.null(x$currentAnalysis$trainingFeatures)){
              out<-.areaStatistics(x$currentAnalysis$trainingFeatures$geometry)
              return(out)

            } else {

              if (!is.null(x$trainingFeatures)){
                out<-.areaStatistics(x$trainingFeatures$geometry)
                return(out)} else {
                  stop(RUNIMC:::mError("couldn't find any training featurs"))
                }
            }
          })

#####-------------------------------------------------------


.roundnessQuantile<-function(fn_trainingFeatures,
                        fn_quantiles){
  aggregate(roundness~label,fn_trainingFeatures,quantile,c(fn_quantiles))
}

if (!isGeneric("tf_roundnessQuantile")) {
  setGeneric("tf_roundnessQuantile", function(x,quantiles,...)
    standardGeneric("tf_roundnessQuantile"))
}

#'
#'
#' @export
setMethod('tf_roundnessQuantile',signature(x='IMC_TrainingFeatures'),
          function(x,quantiles){
            .roundnessQuantile(x$geometry,quantiles)
          })
#'
#'
#' @export
setMethod('tf_roundnessQuantile',signature(x='environment'),
          function(x,quantiles){

            if (!is.null(x$currentAnalysis$trainingFeatures)){
              out<-.roundnessQuantile(x$currentAnalysis$trainingFeatures$geometry,quantiles)
              return(out)

            } else {

              if (!is.null(x$trainingFeatures)){
                out<-.roundnessQuantile(x$trainingFeatures$geometry,quantiles)
                return(out)} else {
                  stop(RUNIMC:::mError("couldn't find any training featurs"))
                }
            }
          })

#####-------------------------------------------------------

.roundnessStatistics<-function(fn_trainingFeatures){
  aggregate(roundness~label,fn_trainingFeatures,summary)

}

if (!isGeneric("tf_roundnessStatistics")) {
  setGeneric("tf_roundnessStatistics", function(x,...)
    standardGeneric("tf_roundnessStatistics"))
}

#'
#'
#' @export
setMethod('tf_roundnessStatistics',signature(x='IMC_TrainingFeatures'),
          function(x){
            .roundnessStatistics(x$geometry)
          })
#'
#'
#' @export
setMethod('tf_roundnessStatistics',signature(x='environment'),
          function(x){

            if (!is.null(x$currentAnalysis$trainingFeatures)){
              out<-.roundnessStatistics(x$currentAnalysis$trainingFeatures$geometry)
              return(out)

            } else {

              if (!is.null(x$trainingFeatures)){
                out<-.roundnessStatistics(x$trainingFeatures$geometry)
                return(out)} else {
                  stop(RUNIMC:::mError("couldn't find any training featurs"))
                }
            }
          })

#####-------------------------------------------------------


.perimeterQuantile<-function(fn_trainingFeatures,
                             fn_quantiles){
  aggregate(perimeter~label,fn_trainingFeatures,quantile,c(fn_quantiles))
}

if (!isGeneric("tf_perimeterQuantile")) {
  setGeneric("tf_perimeterQuantile", function(x,quantiles,...)
    standardGeneric("tf_perimeterQuantile"))
}

#'
#'
#' @export
setMethod('tf_perimeterQuantile',signature(x='IMC_TrainingFeatures'),
          function(x,quantiles){
            .perimeterQuantile(x$geometry,quantiles)
          })
#'
#'
#' @export
setMethod('tf_perimeterQuantile',signature(x='environment'),
          function(x,quantiles){

            if (!is.null(x$currentAnalysis$trainingFeatures)){
              out<-.perimeterQuantile(x$currentAnalysis$trainingFeatures$geometry,quantiles)
              return(out)

            } else {

              if (!is.null(x$trainingFeatures)){
                out<-.perimeterQuantile(x$trainingFeatures$geometry,quantiles)
                return(out)} else {
                  stop(RUNIMC:::mError("couldn't find any training featurs"))
                }
            }
          })

#####-------------------------------------------------------

.perimeterStatistics<-function(fn_trainingFeatures){
  aggregate(perimeter~label,fn_trainingFeatures,summary)

}

if (!isGeneric("tf_perimeterStatistics")) {
  setGeneric("tf_perimeterStatistics", function(x,...)
    standardGeneric("tf_perimeterStatistics"))
}

#'
#'
#' @export
setMethod('tf_perimeterStatistics',signature(x='IMC_TrainingFeatures'),
          function(x){
            .perimeterStatistics(x$geometry)
          })
#'
#'
#' @export
setMethod('tf_perimeterStatistics',signature(x='environment'),
          function(x){

            if (!is.null(x$currentAnalysis$trainingFeatures)){
              out<-.perimeterStatistics(x$currentAnalysis$trainingFeatures$geometry)
              return(out)

            } else {

              if (!is.null(x$trainingFeatures)){
                out<-.perimeterStatistics(x$trainingFeatures$geometry)
                return(out)} else {
                  stop(RUNIMC:::mError("couldn't find any training featurs"))
                }
            }
          })

#####-------------------------------------------------------

.labelList<-function(fn_trainingFeatures){
  if (class(fn_trainingFeatures$geometry$label)=='factor'){
  out<-levels(fn_trainingFeatures$geometry$label)} else {
    out<-unique(fn_trainingFeatures$geometry$label)
  }
  return(out)
}

if (!isGeneric("tf_labelList")) {
  setGeneric("tf_labelList", function(x,...)
    standardGeneric("tf_labelList"))
}

#'
#'
#' @export
setMethod('tf_labelList',signature(x='IMC_TrainingFeatures'),
          function(x){
            .labelList(x)
          })
#'
#'
#' @export
setMethod('tf_labelList',signature(x='environment'),
          function(x){

            if (!is.null(x$currentAnalysis$trainingFeatures)){
              out<-.labelList(x$currentAnalysis$trainingFeatures)
              return(out)

            } else {

              if (!is.null(x$trainingFeatures)){
                out<-.labelList(x$trainingFeatures)
                return(out)} else {
                  stop(RUNIMC:::mError("couldn't find any training featurs"))
                }
            }
          })





