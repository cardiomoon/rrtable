% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data2plotzip.R
\name{data2plotzip}
\alias{data2plotzip}
\title{Make zipped plot file with a data.frame}
\usage{
data2plotzip(
  data,
  path = ".",
  filename = "Plot.zip",
  format = "PNG",
  width = 8,
  height = 6,
  units = "in",
  res = 300,
  start = 0,
  preprocessing = "",
  rawDataName = NULL,
  rawDataFile = "rawData.RDS",
  out = NULL
)
}
\arguments{
\item{data}{A data.frame}

\item{path}{A name of destination file path}

\item{filename}{A path of destination file}

\item{format}{Plot format. Choices are c("PNG","SVG","PDF")}

\item{width}{A plot width}

\item{height}{A plot height}

\item{units}{The units in which height and width are given. Can be px (pixels, the default), in (inches), cm or mm.}

\item{res}{The nominal resolution in ppi}

\item{start}{Plot start number}

\item{preprocessing}{A character string of R code}

\item{rawDataName}{The name of the rawData}

\item{rawDataFile}{The name of the rawData file which the data are to be read from.}

\item{out}{An object or NULL}
}
\description{
Make zipped plot file with a data.frame
}
\examples{
\dontrun{
library(moonBook)
library(ztable)
library(rrtable)
library(ggplot2)
data2plotzip(sampleData2,path="tmp")
}
}
