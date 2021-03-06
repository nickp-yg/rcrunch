% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/auth.R
\name{session}
\alias{session}
\title{Get various catalogs for your Crunch session}
\usage{
session()
}
\value{
A Session object. Access dataset and project catalogs from it.
}
\description{
Get various catalogs for your Crunch session
}
\examples{
\dontrun{
cr <- session()
cr$datasets
cr$projects
}
}
