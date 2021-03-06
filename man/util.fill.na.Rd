% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils-prep.R
\name{util.fill.na}
\alias{util.fill.na}
\title{fill NA with constant value or function}
\usage{
util.fill.na(x, fill.with = 0)
}
\arguments{
\item{x}{input array}

\item{fill.with}{constant value or function}
}
\value{
array with filled na
}
\description{
fill NA with constant value or function
}
\examples{
util.fill.na(c(rnorm(10), NA), fill.with = 100)
util.fill.na(c(rnorm(10), NA), fill.with = length)
}
