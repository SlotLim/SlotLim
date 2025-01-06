#' TBA
#'
#' @description
#' This function calculates the Targeted Biomass Adjustment (TBA), which dampens the influence of biomass indices on catch advice when the proportion of the size range targeted by harvest slot limits is small.
#'
#' @param P_targeted The proportion of the size range targeted by harvest slot limits, calculated using the `prop_target` function.
#' @param rB The proportional rate of change in a biomass index, calculated using the `rB` function.
#'
#' @return The targeted biomass adjustment. When TBA > 1, catch advice increases, When TBA < 1, catch advice decreases.
#'
#' @examples
#' P_targeted <- 0.5
#' rB <- -0.5
#'
#' TBA(P_targeted, rB)
#'
#' @seealso \code{\link{prop_target}} # for the proportion targeted by HSL
#' @seealso \code{\link{rB}} # for the proportional rate of change in a biomass
#'
#' @export
TBA <- function(P_targeted, rB) {

  # Validate inputs
  if (!is.numeric(P_targeted) || !is.numeric(rB)) {
    stop("All inputs must be numeric.")
  }
  if (P_targeted < 0 || P_targeted > 1) {
    stop("P_targeted must be between 0 and 1.")
  }
  if (rB < -1 || rB > 1) {
    warning("rB should typically be between -1 and 1, but can be outside this range for extreme cases.")
  }

  # Dampen rB using P_targeted
  damp_rB <- rB * P_targeted

  # Calculate the advised catch multiplier
  multiplier <- 1 + damp_rB

  return(multiplier)
}
