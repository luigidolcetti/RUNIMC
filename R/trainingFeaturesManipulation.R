.areaQuantile<-function(fn_trainingFeatures,
                        fn_quantiles){
  aggregate(area~label,fn_trainingFeatures,quantile,c(fn_quantiles))
}

#' Training features
#'
#' Calculates specified quantiles of area for each label.
#'
#' @param x environment, IMC_TrainingFeatures, a study or the object storing training features
#' @param quantiles numeric, vector of quantiles to be calculated
#' @return numeric matrix
#' @export
setGeneric("tf_areaQuantile", function(x,quantiles,...)
  standardGeneric("tf_areaQuantile"))

setMethod('tf_areaQuantile',signature(x='IMC_TrainingFeatures'),
          function(x,quantiles){
            .areaQuantile(x$geometry,quantiles)
          })

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

#' Training features
#'
#' Calculates area summary statistics for each label.
#'
#' @param x environment, IMC_TrainingFeatures, a study or the object storing training features
#' @param quantiles numeric, vector of quantiles to be calculated
#' @return numeric matrix
#' @export
setGeneric("tf_areaStatistics", function(x,...)
  standardGeneric("tf_areaStatistics"))

setMethod('tf_areaStatistics',signature(x='IMC_TrainingFeatures'),
          function(x){
            .areaStatistics(x$geometry)
          })

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

#' Training features
#'
#' Calculates specified quantiles of roundness for each label.
#'
#' @param x environment, IMC_TrainingFeatures, a study or the object storing training features
#' @param quantiles numeric, vector of quantiles to be calculated
#' @return numeric matrix
#' @export
setGeneric("tf_roundnessQuantile", function(x,quantiles,...)
  standardGeneric("tf_roundnessQuantile"))

setMethod('tf_roundnessQuantile',signature(x='IMC_TrainingFeatures'),
          function(x,quantiles){
            .roundnessQuantile(x$geometry,quantiles)
          })

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

#' Training features
#'
#' Calculates roundness summary statistics for each label.
#'
#' @param x environment, IMC_TrainingFeatures, a study or the object storing training features
#' @param quantiles numeric, vector of quantiles to be calculated
#' @return numeric matrix
#' @export
setGeneric("tf_roundnessStatistics", function(x,...)
  standardGeneric("tf_roundnessStatistics"))

setMethod('tf_roundnessStatistics',signature(x='IMC_TrainingFeatures'),
          function(x){
            .roundnessStatistics(x$geometry)
          })

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

#' Training features
#'
#' Calculates specified quantiles of perimeter for each label.
#'
#' @param x environment, IMC_TrainingFeatures, a study or the object storing training features
#' @param quantiles numeric, vector of quantiles to be calculated
#' @return numeric matrix
#' @export
setGeneric("tf_perimeterQuantile", function(x,quantiles,...)
  standardGeneric("tf_perimeterQuantile"))

setMethod('tf_perimeterQuantile',signature(x='IMC_TrainingFeatures'),
          function(x,quantiles){
            .perimeterQuantile(x$geometry,quantiles)
          })

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

#' Training features
#'
#' Calculates perimeter summary statistics for each label.
#'
#' @param x environment, IMC_TrainingFeatures, a study or the object storing training features
#' @param quantiles numeric, vector of quantiles to be calculated
#' @return numeric matrix
#' @export
setGeneric("tf_perimeterStatistics", function(x,...)
  standardGeneric("tf_perimeterStatistics"))

setMethod('tf_perimeterStatistics',signature(x='IMC_TrainingFeatures'),
          function(x){
            .perimeterStatistics(x$geometry)
          })

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

#' Training features
#'
#' Lists labels present in the training features table
#'
#' @param x environment, IMC_TrainingFeatures, a study or the object storing training features
#' @return numeric vector
#' @export
setGeneric("tf_labelList", function(x,...)
  standardGeneric("tf_labelList"))


setMethod('tf_labelList',signature(x='IMC_TrainingFeatures'),
          function(x){
            .labelList(x)
          })

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


#####-------------------------------------------------------

.featuresList<-function(fn_trainingFeatures_value){
  nonFeaturesColumns<-c('uid',
                        'pixel.id',
                        'polygon.id',
                        'parLabel',
                        'label',
                        'DFC',
                        'SLI',
                        'x',
                        'y',
                        'coverage_fraction')
  out<-colnames(fn_trainingFeatures_value[,!(colnames(fn_trainingFeatures_value) %in% nonFeaturesColumns)])
  return(out)
}

#' Training features
#'
#' Lists features present in the training features table
#'
#' @param x environment, IMC_TrainingFeatures, a study or the object storing training features
#' @return numeric vector
#' @export
setGeneric("tf_featureList", function(x,...)
  standardGeneric("tf_featureList"))

setMethod('tf_featureList',signature(x='IMC_TrainingFeatures'),
          function(x){
            .featuresList(x$value)
          })

setMethod('tf_featureList',signature(x='environment'),
          function(x){

            if (!is.null(x$currentAnalysis$trainingFeatures$value)){
              out<-.featuresList(x$currentAnalysis$trainingFeatures$value)
              return(out)

            } else {

              if (!is.null(x$trainingFeatures$value)){
                out<-.featuresList(x$trainingFeatures$value)
                return(out)} else {
                  stop(RUNIMC:::mError("couldn't find any training featurs"))
                }
            }
          })




