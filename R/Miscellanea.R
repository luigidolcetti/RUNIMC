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


methodParametersClassification<-list(
  randomForest = list(

    responseVariable = 'label',
    predictiveFeatures = NULL ,
    PvalueTreshold=0,
    ntree = NULL,
    mtry = NULL,
    max = NULL,
    replace = NULL,
    classwt = NULL,
    cutoff = NULL,
    strata = NULL,
    samplesize = NULL,
    nodesize = NULL,
    maxnodes = NULL,
    importance = NULL,
    localImp = NULL,
    nPerm = NULL,
    proximity = NULL,
    oob.prox = NULL,
    norm.vote = NULL,
    do.trace = NULL,
    keep.forest = NULL,
    corr.bias = NULL,
    keep.inbag = NULL,
    ... = NULL),

  randomOnions = list(
    responseVariable = 'label',
    predictiveFeatures = NULL ,
    labels = NULL,
    classificationLyr = 'label',
    prefix = 'topoMap_',
    ntree = NULL,
    mtry = NULL,
    max = NULL,
    replace = NULL,
    classwt = NULL,
    cutoff = NULL,
    strata = NULL,
    samplesize = NULL,
    nodesize = NULL,
    maxnodes = NULL,
    importance = NULL,
    localImp = NULL,
    nPerm = NULL,
    proximity = NULL,
    oob.prox = NULL,
    norm.vote = NULL,
    do.trace = NULL,
    keep.forest = NULL,
    corr.bias = NULL,
    keep.inbag = NULL,
    ... = NULL)

)

methodParametersSegmentation<-list(
  spiderMap = list(areaQuantile=c(0,1),
                   roundnessQuantile=c(0,1),
                   areaExpansion=c(1,1),
                   roundnessExpansion = c(1,1),
                   spikes=8,
                   radiusExpansion = 1.5,
                   densityMultiplier=1,
                   coverage = 0.5,
                   seedOutScore = 3,
                   cutSeedList = 0.05,
                   cycleWindow = 1000,
                   discoverTreshold = 1e-3,
                   adaptative = T,
                   drasticExpansion = 0.75,
                   direction = 'random',
                   seed=123),
  ratMap = list(areaQuantile=c(0,1),
                roundnessQuantile=c(0,1),
                areaExpansion=c(1,1),
                roundnessExpansion = c(1,1),
                spikes=8,
                radiusExpansion = 1.5,
                coverage = 0.5,
                seedOutScore = 3,
                cycleWindow = 1000,
                discoverTreshold = 1e-3,
                adaptative = T,
                drasticExpansion = 0.5,
                lowPenalty=2,
                highPenalty=1,
                roundnessPenalty=1,
                seed=123),
  slothMap = list(areaQuantile=c(0,1),
                  roundnessQuantile=c(0,1),
                  spikes=8,
                  radiusExpansion=1.5,
                  coverage=0.3,
                  seedOutScore=10,
                  cycleWindow=1000,
                  discoverTreshold=1e-3,
                  adaptative=T,
                  areaAdaptRate=0.1,
                  roundnessAdaptRate=0.1,
                  fusion=T,
                  targetArea='training_mean',
                  maxNetworkSize=8,
                  inflateDeflate=0.1,
                  favourForeing=T,
                  returnKinetic=T,
                  returnRasters=T),
  alligatorMap = list(areaQuantile=c(0,1),
                      roundnessQuantile=c(0,1),
                      spikes=8,
                      radiusExpansion=1.5,
                      coverage=0.3,
                      seedOutScore=10,
                      cycleWindow=100,
                      adaptative=T,
                      areaAdaptRate=0.1,
                      roundnessAdaptRate=0.1,
                      segmentAlg = 'sign',
                      fusion=T,
                      targetArea='training_mean',
                      maxNetworkSize=8,
                      inflateDeflate=0.1,
                      favourForeing=T,
                      returnKinetic=T,
                      returnRasters=T),
  lazyCatMap = list(indexToExclude = NULL)
)
