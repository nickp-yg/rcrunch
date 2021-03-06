% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/new-dataset.R
\name{writePreparedData}
\alias{writePreparedData}
\title{Persist to disk a prepared data.frame and metadata}
\usage{
writePreparedData(data, metadata = attr(data, "metadata"), file)
}
\arguments{
\item{data}{a data.frame that meets the Crunch API specification, as returned
by \code{\link{prepareDataForCrunch}}.}

\item{metadata}{list of Crunch metadata that corresponds to \code{data}.
Default is the "metadata" attribute of \code{data}, as returned by
\code{prepareDataForCrunch}.}

\item{file}{character file path, without extension, to write to.}
}
\value{
A character vector of length 2: given a value of
\code{file="example"}, it would return c("example.csv.gz", "example.json").
The function, of course, is called for its side effects of writing a gzipped
CSV and a JSON file to those locations.
}
\description{
Persist to disk a prepared data.frame and metadata
}
