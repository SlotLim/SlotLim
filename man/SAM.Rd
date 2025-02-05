% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SAM.R
\name{SAM}
\alias{SAM}
\title{SAM}
\usage{
SAM(L_2.5, L_97.5, MinLS, MaxLS)
}
\arguments{
\item{L_2.5}{The 2.5th percentile of the catch from fishery-dependent length-frequency data.}

\item{L_97.5}{The 97.5th percentile of the catch from fishery-dependent length-frequency data.}

\item{MinLS}{The minimum landing size.}

\item{MaxLS}{The maximum landing size.}
}
\value{
A list containing the following elements:
\describe{
\item{lower_adherence}{The relative deviation of the 2.5th percentile (\code{L_2.5}) from the minimum landing size (\code{MinLS}).}
\item{upper_adherence}{The relative deviation of the 97.5th percentile (\code{L_97.5}) from the maximum landing size (\code{MaxLS}).}
\item{SAM}{The size adherence multiplier. When SAM > 1, catch advice increases, When SAM < 1, catch advice decreases.}
}
}
\description{
This function calculates the Size Adherence Multiplier (SAM), which evaluates the adherence of the catch distribution to harvest slot limits.
When \code{L_2.5 >= MinLS} and \code{L_97.5 <= MaxLS}, neither harvest slot limits are violated and the multiplier is calculated without constraint.
When \code{L_2.5 < MinLS} or \code{L_97.5 > MaxLS}, harvest slot limits have been violated and the multiplier is constrained to a maximum of \code{1}.
}
\examples{
L_2.5 <- 15     # 2.5th percentile
L_97.5 <- 35    # 97.5th percentile
MinLS <- 20     # Minimum landing size
MaxLS <- 40     # Maximum landing size

SAM(L_2.5, L_97.5, MinLS, MaxLS)

}
\seealso{
\code{\link{percentile}} for calculating \code{L_2.5} and \code{L_97.5} from length-frequency data.
}
