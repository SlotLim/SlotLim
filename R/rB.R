#' rB
#'
#' @description
#' This function calculates the proportional rate of change in a biomass index (rB) between consecutive data points using one of three available methods.
#' \describe{
#'   \item{`annual`}{Calculates the proportional rate of change between two consecutive data points.}
#'   \item{`1over2`}{Calculates the proportional rate of change by comparing the most recent data point to the average of the next two data points.}
#'   \item{`2over3`}{Calculates the proportional rate of change using the average of the first two data points, compared to the average of the next three data points.}
#' }
#'
#' @param data Consecutive biomass index data (e.g., CPUE values). Data must be in descending order, with the most recent value first.
#' @param method The calculation method (`"annual"`, `"1over2"`, or `"2over3"`). Default is `"annual"`.
#'
#' @return The proportional rate of change in the biomass index. A positive `rB` indicates an increase in the biomass index, while a negative `rB` indicates a decrease.
#'
#' @examples
#' biomass <- c(100, 120, 110, 130, 140)
#' rB(biomass, method = "annual")
#' rB(biomass, method = "1over2")
#' rB(biomass, method = "2over3")
#'
#' @export
rB <- function(data, method = "annual") {

  # Validate input
  if (!is.numeric(data) || length(data) < 2) {
    stop("Input data must be a numeric vector with at least two values.")
  }

  # Perform calculation based on method
  rB_value <- switch(
    method,
    "annual" = {
      if (length(data) < 2) stop("The 'annual' method requires two consecutive data points.")
      (data[1] - data[2]) / data[2]  # Proportional change
    },
    "1over2" = {
      if (length(data) < 3) stop("The '1over2' method requires three consecutive data points.")
      (data[1] - mean(data[2:3])) / mean(data[2:3])  # Proportional change
    },
    "2over3" = {
      if (length(data) < 5) stop("The '2over3' method requires five consecutive data points.")
      (mean(data[1:2]) - mean(data[3:5])) / mean(data[3:5])  # Proportional change
    },
    stop("Invalid method. Choose 'annual', '1over2', or '2over3'.")
  )

  return(rB_value)
}
