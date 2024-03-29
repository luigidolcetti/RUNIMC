% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/imageRFilter.R
\name{imageRFilter}
\alias{imageRFilter}
\title{Filters}
\usage{
imageRFilter(
  fn_rasterStack,
  fn_filter,
  fn_markerList,
  fn_filterParameterList,
  fn_saveToDisk,
  fn_pathToFile
)
}
\arguments{
\item{fn_rasterStack}{IMC_RasterStack to be processed.}

\item{fn_filter}{character, filter to be applied. Available filters are
vanvliet, deriche or blur_anisotropic.}

\item{fn_markerList}{character, vector name of layers to be processed.}

\item{fn_filterParameterList}{list, parameter to be passed to the filter
function.}

\item{fn_saveToDisk}{logical, save to disk? Always advaisable.}

\item{fn_pathToFile}{character, path to a folder that will contain
the subfolder rasters and rasterStacks}
}
\value{
a new rasterstack with processed rasters

what.
}
\description{
\code{\link[=imageRFilter]{imageRFilter()}} is the wrapping function that apply \code{\link[imager:vanvliet]{imager::vanvliet()}},
\code{\link[imager:deriche]{imager::deriche()}} or \code{\link[imager:blur_anisotropic]{imager::blur_anisotropic()}} to a rater.
}
