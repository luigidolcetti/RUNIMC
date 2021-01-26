
setOldClass('randomForest')
setOldClass('randomForest.formula')
setOldClass('sf')
#* IMC_RsCollection ---------------------------------------------------
#' A Class container for IMC_RasterStaks
#' Namely a collection of images from different histological preparations.
IMC_RsCollection<-setClass('IMC_RsCollection',
                           contains = 'list')

#* IMC_SegmentationList ---------------------------------------------------
#' A Class container for the results of segmentation
IMC_SegmentationList<-setClass('IMC_SegmentationList',
                               contains = 'list')

#* IMC_InterpretationMatrix ---------------------------------------------------
#' A Class container directives
IMC_InterpretationMatrix<-setClass('IMC_InterpretationMatrix',
                                   contains = 'list')

#* IMC_Classification ---------------------------------------------------
#' A Class container directives
IMC_Classification<-setClass('IMC_Classification',
                             contains = 'list')

#* IMC_Classifier ---------------------------------------------------
#' A Class container directives
IMC_Classifier<-setClassUnion('IMC_Classifier',members =c('randomForest.formula','randomForest'))

#* IMC_TrainingFeatures ---------------------------------------------------
#' A Class container directives
IMC_TrainingFeatures<-setClass('IMC_TrainingFeatures',
                               contains = 'list')

#* IMC_StudyTable ---------------------------------------------------
#' A Class container directives
IMC_StudyTable<-setClass('IMC_StudyTable',
                         contains = 'data.frame')

#* IMC_ChannelTable ---------------------------------------------------
#' A Class container directives
IMC_ChannelTable<-setClass('IMC_ChannelTable',
                           contains = 'data.frame')

#* IMC_RasterStack ---------------------------------------------------
#' A Class container directives
IMC_RasterStack<-setClass('IMC_RasterStack',
                          contains = 'RasterStack',
                          representation(uid='character',
                                         IMC_text_file='character',
                                         study='character',
                                         sample='character',
                                         replicate='character',
                                         ROI='character',
                                         bioGroup='character',
                                         channels = 'data.frame'),
                          prototype  (uid='',
                                      IMC_text_file='',
                                      study='',
                                      sample='',
                                      replicate='',
                                      ROI='',
                                      bioGroup='',
                                      channels=data.frame(columnNames=character(0),
                                                          RcolumnNames=character(0),
                                                          channel=character(0),
                                                          marker=character(0),
                                                          loaded=logical(0),
                                                          stringsAsFactors = F)))


#* IMC_segmentation ---------------------------------------------------
#' A Class container directives
IMC_Segmentation<-setClass('IMC_Segmentation',
                           representation(polygons='list',
                                          performance='data.frame',
                                          raster='RasterLayer'))

#* IMC_FilterFrame ---------------------------------------------------
#' A Class container directives
IMC_FilterFrame<-setClass('IMC_FilterFrame',
                          contains = 'data.frame',
                          representation(),
                          prototype(data.frame(
                            filter=character(),
                            parameters=list(),
                            channels=c()
                          )))

#* IMC_ExtractionDirectives ---------------------------------------------------
#' A Class container directives
IMC_ExtractionDirectives<-setClass('IMC_ExtractionDirectives',
                                   contains = 'data.frame',
                                   representation(),
                                   prototype(data.frame(
                                     coverage=c(),
                                     prefix=c())))

#* IMC_ClassificationDirectives ---------------------------------------------------
#' A Class container directives
IMC_ClassificationDirectives<-setClass('IMC_ClassificationDirectives',
                                       representation(method='character',
                                                      methodParameters='list'),
                                       prototype(method=c(),
                                                 methodParameters=list()))

#* IMC_SegmentationDirectives ---------------------------------------------------
#' A Class container directives
IMC_SegmentationDirectives<-setClass('IMC_SegmentationDirectives',
                                     representation(method='character',
                                                    methodParameters='list'),
                                     prototype(method=c(),
                                               methodParameters=list()))

#* IMC_AnalysisList ---------------------------------------------------
#' A Class container directives
IMC_AnalysisList<-setClass('IMC_AnalysisList',
                           contains = 'list')
