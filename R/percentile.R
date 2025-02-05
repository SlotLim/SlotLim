#' percentile
#'
#' @importFrom stats quantile
#'
#' @description
#' This function calculates the 2.5th and 97.5th percentiles from length-frequency data.
#'
#' @param LF length-frequency data (e.g., `data$length`).
#' @return A list with two named elements:
#' \describe{
#'   \item{L_2.5}{The 2.5th percentile of the catch.}
#'   \item{L_97.5}{The 97.5th percentile of the catch.}
#' }
#'
#' @examples
#' length_data <- c(9, 10, 10, 10, 11, 11, 13, 14, 15, 19)
#' percentiles <- percentile(length_data)
#' L_2.5 <- percentiles[[1]]
#' L_97.5 <- percentiles[[2]]
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
  L_2.5 <- quantile(LF, probs = 0.025, na.rm = TRUE)
  L_97.5 <- quantile(LF, probs = 0.975, na.rm = TRUE)

  # Return the results as a list
  return(list(L_2.5 = L_2.5, L_97.5 = L_97.5))
}
