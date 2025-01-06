#' prop_target
#'
#' @description
#' The function calculates the proportion of the size range targeted by harvest slot limits.
#'
#' @param MinLS The minimum landing size.
#' @param MaxLS The maximum landing size.
#' @param L_max The maximum observed length.
#'
#' @return The proportion of the size range targeted by harvest slot limits.
#'
#' @examples
#' MinLS <- 50     # Minimum landing size
#' MaxLS <- 100    # Maximum landing size
#' L_max <- 200    # Maximum observed length
#'
#' P_targeted <- prop_target(MinLS, MaxLS, L_max)
#' print(P_targeted)
#'
#' @export
prop_target <- function(MinLS, MaxLS, L_max) {

  # Validate inputs
  if (!is.numeric(c(MinLS, MaxLS, L_max))) {
    stop("All inputs must be numeric.")
  }
  if (any(c(MinLS, MaxLS, L_max) < 0)) {
    stop("Lengths must be non-negative.")
  }
  if (MinLS >= MaxLS) {
    stop("Minimum landing size (MinLS) must be less than maximum landing size (MaxLS).")
  }

  # Calculate the proportion of size range targeted
  P_targeted <- (MaxLS - MinLS) / L_max

  # Ensure the proportion is within bounds [0, 1]
  if (P_targeted < 0 | P_targeted > 1) {
    warning("The calculated proportion (P_targeted) is outside the range [0, 1]. Check your input values.")
  }

  # Return
  return(P_targeted = round(P_targeted, 2)
  )
}
