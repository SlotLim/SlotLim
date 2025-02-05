% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/percentile.R
\name{percentile}
\alias{percentile}
\title{percentile}
\usage{
percentile(LF)
}
\arguments{
\item{LF}{length-frequency data (e.g., \code{data$length}).}
}
\value{
A list with two named elements:
\describe{
\item{L_2.5}{The 2.5th percentile of the catch.}
\item{L_97.5}{The 97.5th percentile of the catch.}
}
}
\description{
This function calculates the 2.5th and 97.5th percentiles from length-frequency data.
}
\examples{
length_data <- c(9, 10, 10, 10, 11, 11, 13, 14, 15, 19)
percentiles <- percentile(length_data)
L_2.5 <- percentiles[[1]]
L_97.5 <- percentiles[[2]]

}
