#' percentile
#'
#' @importFrom stats quantile
#'
#' @description
#' This function calculates the 5th and 95th percentiles from length-frequency data.
#'
#' @param LF length-frequency data (e.g., `data$length`).
#' @return A list with two named elements:
#' \describe{
#'   \item{L_5}{The 5th percentile of the catch.}
#'   \item{L_95}{The 95th percentile of the catch.}
#' }
#'
#' @examples
#' length_data <- c(9, 10, 10, 10, 11, 11, 13, 14, 15, 19)
#' percentiles <- percentile(length_data)
#' L_5 <- percentiles[[1]]
#' L_95 <- percentiles[[2]]
#'
#' @export
percentile <- function(LF) {
  # Validate input
  if (!is.numeric(LF)) {
    stop("Input data must be a numeric vector.")
  }

  if (length(LF) < 2) {
    stop("Input data must contain at least two values.")
  }

  # Calculate percentiles
  L_5 <- quantile(LF, probs = 0.05, na.rm = TRUE)
  L_95 <- quantile(LF, probs = 0.95, na.rm = TRUE)

  # Return the results as a list
  return(list(L_5 = L_5, L_95 = L_95))
}
