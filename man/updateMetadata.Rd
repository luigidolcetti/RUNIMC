% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/updateMetadata.R
\name{updateMetadata}
\alias{updateMetadata}
\title{Update study metadata}
\usage{
updateMetadata(x, saveToDisk, ...)
}
\arguments{
\item{x}{environment, representing a study}

\item{saveToDisk}{logic, save updated rasterStaks and raster collection}
}
\value{
the method act in place on the correct object within the specified study environment
}
\description{
Update metadata in raster collections such as the raw data, additional raster created via filter application and classification rasters.
}
\examples{
\dontrun{
updateMetadata( x = MyStudy, saveToDisk = T)
}

}
