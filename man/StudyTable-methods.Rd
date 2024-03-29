% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/studyTable_methods.R
\docType{methods}
\name{studyTable}
\alias{studyTable}
\alias{studyTable,environment-method}
\alias{StudyTable,environment-method}
\alias{st_uids}
\alias{st_uids,environment-method}
\alias{st_uids,environment}
\alias{st_uids,IMC_StudyTable-method}
\alias{st_uids,IMC_StudyTable}
\alias{st_uids<-}
\alias{st_uids<-,environment-method}
\alias{st_uids<-,environment}
\alias{st_uids<-,IMC_StudyTable-method}
\alias{st_uids<-,IMC_StudyTable}
\alias{st_folders}
\alias{st_folders,environment-method}
\alias{st_folders,environment}
\alias{st_folders,IMC_StudyTable-method}
\alias{st_folders,IMC_StudyTable}
\alias{st_folders<-}
\alias{st_folders<-,environment-method}
\alias{st_folders<-,environment}
\alias{st_folders<-,IMC_StudyTable-method}
\alias{st_folders<-,IMC_StudyTable}
\alias{st_files}
\alias{st_files,environment-method}
\alias{st_files,environment}
\alias{st_files,IMC_StudyTable-method}
\alias{st_files,IMC_StudyTable}
\alias{st_files<-}
\alias{st_files<-,environment-method}
\alias{st_files<-,environment}
\alias{st_files<-,IMC_StudyTable-method}
\alias{st_files<-,IMC_StudyTable}
\alias{st_samples}
\alias{st_samples,environment-method}
\alias{samples,environment}
\alias{st_samples,IMC_StudyTable-method}
\alias{IMCfile,IMC_StudyTable}
\alias{st_samples<-}
\alias{st_samples<-,environment-method}
\alias{st_samples<-,environment}
\alias{st_samples<-,IMC_StudyTable-method}
\alias{st_samples<-,IMC_StudyTable}
\alias{st_replicates}
\alias{st_replicates,environment-method}
\alias{st_replicates,environment}
\alias{st_replicates,IMC_StudyTable-method}
\alias{st_replicates,IMC_StudyTable}
\alias{st_replicates<-}
\alias{st_replicates<-,environment-method}
\alias{st_replicates<-,environment}
\alias{st_replicates<-,IMC_StudyTable-method}
\alias{st_replicates<-,IMC_StudyTable}
\alias{st_rois}
\alias{st_rois,environment-method}
\alias{st_rois,environment}
\alias{st_rois,IMC_StudyTable-method}
\alias{st_rois,IMC_StudyTable}
\alias{st_rois<-}
\alias{st_rois<-,environment-method}
\alias{st_rois<-,environment}
\alias{st_rois<-,IMC_StudyTable-method}
\alias{st_rois<-,IMC_StudyTable}
\alias{st_bioGroups}
\alias{st_bioGroups,environment-method}
\alias{st_bioGroups,environment}
\alias{st_bioGroups,IMC_StudyTable-method}
\alias{st_bioGroups,IMC_StudyTable}
\alias{st_bioGroups<-}
\alias{st_bioGroups<-,environment-method}
\alias{st_bioGroups<-,environment}
\alias{st_bioGroups<-,IMC_StudyTable-method}
\alias{st_bioGroups<-,IMC_StudyTable}
\title{Study Table content}
\usage{
studyTable(x, ...)

\S4method{studyTable}{environment}(x, ...)

st_uids(x, ...)

\S4method{st_uids}{environment}(x, ...)

\S4method{st_uids}{IMC_StudyTable}(x, ...)

st_uids(x, ...) <- value

\S4method{st_uids}{environment}(x, ...) <- value

\S4method{st_uids}{IMC_StudyTable}(x, ...) <- value

st_folders(x, ...)

\S4method{st_folders}{environment}(x, ...)

\S4method{st_folders}{IMC_StudyTable}(x, ...)

st_folders(x, ...) <- value

\S4method{st_folders}{environment}(x, ...) <- value

\S4method{st_folders}{IMC_StudyTable}(x, ...) <- value

st_files(x, ...)

\S4method{st_files}{environment}(x, ...)

\S4method{st_files}{IMC_StudyTable}(x, ...)

st_files(x, ...) <- value

\S4method{st_files}{environment}(x, ...) <- value

\S4method{st_files}{IMC_StudyTable}(x, ...) <- value

st_samples(x, ...)

\S4method{st_samples}{environment}(x, ...)

\S4method{st_samples}{IMC_StudyTable}(x, ...)

st_samples(x, ...) <- value

\S4method{st_samples}{environment}(x, ...) <- value

\S4method{st_samples}{IMC_StudyTable}(x, ...) <- value

st_replicates(x, ...)

\S4method{st_replicates}{environment}(x, ...)

\S4method{st_replicates}{IMC_StudyTable}(x, ...)

st_replicates(x, ...) <- value

\S4method{st_replicates}{environment}(x, ...) <- value

\S4method{st_replicates}{IMC_StudyTable}(x, ...) <- value

st_rois(x, ...)

\S4method{st_rois}{environment}(x, ...)

\S4method{st_rois}{IMC_StudyTable}(x, ...)

st_rois(x, ...) <- value

\S4method{st_rois}{environment}(x, ...) <- value

\S4method{st_rois}{IMC_StudyTable}(x, ...) <- value

st_bioGroups(x, ...)

\S4method{st_bioGroups}{environment}(x, ...)

\S4method{st_bioGroups}{IMC_StudyTable}(x, ...)

st_bioGroups(x, ...) <- value

\S4method{st_bioGroups}{environment}(x, ...) <- value

\S4method{st_bioGroups}{IMC_StudyTable}(x, ...) <- value
}
\value{
an object of class IMC_StudyTable or a character vector (named if different from uids)
}
\description{
Retrive or update content of the study table. Some content such as identifiers
are read only to prevent corruption of study cross references.
}
\details{
studyTable: retrive the entire study table, there is no assignment method

st_uids: retrive uid column

st_uids<-: prevent uid modifications

st_folders: retrive IMC_folder column

st_folders<-: prevent IMCfolder modifications

st_files: retrive IMC_text_file column

st_files<-: prevent folder modifications

st_samples: retrive IMC_text_file column

samples<-: modify sample names

st_replicates: retrive IMC_text_file column

st_replicates<-: modify replicate names

IMCfile: retrive IMC_text_file column

st_rois<-: modify region of interest (ROI) names

IMCfile: retrive IMC_text_file column

st_bioGroups<-: modify bio group (eg. healthy control, treated, tumor... ) names
}
\examples{
\dontrun{
default_study<-studyTable(x = MyStudy)
samples(x = myStudy)<-('MySample1','MySample2','MySample3')
}
}
\seealso{
updateMetadata() for extend updated values to other objects in the study
}
