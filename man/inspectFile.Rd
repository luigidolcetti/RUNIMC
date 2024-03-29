% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inspectFile.R
\name{inspectFile}
\alias{inspectFile}
\title{Inspect}
\usage{
inspectFile(fn_file = NULL)
}
\arguments{
\item{fn_file}{character, path to a txt tab-delimited raw data table.}
}
\value{
a character string that can be printed with \strong{\code{\link[=cat]{cat()}}}.
}
\description{
Inspect a raw data table for possible problems related to channel and markers
names, coordinates, missing data.
}
