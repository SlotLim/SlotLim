#' SAM
#'
#' @description
#' This function calculates the Size Adherence Multiplier (SAM), which evaluates the adherence of the catch distribution to harvest slot limits.
#' When `L_2.5 >= MinLS` and `L_97.5 <= MaxLS`, neither harvest slot limits are violated and the multiplier is calculated without constraint.
#' When `L_2.5 < MinLS` or `L_97.5 > MaxLS`, harvest slot limits have been violated and the multiplier is constrained to a maximum of `1`.
#'
#' @param L_2.5 The 2.5th percentile of the catch from fishery-dependent length-frequency data.
#' @param L_97.5 The 97.5th percentile of the catch from fishery-dependent length-frequency data.
#' @param MinLS The minimum landing size.
#' @param MaxLS The maximum landing size.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{lower_adherence}{The relative deviation of the 2.5th percentile (`L_2.5`) from the minimum landing size (`MinLS`).}
#'   \item{upper_adherence}{The relative deviation of the 97.5th percentile (`L_97.5`) from the maximum landing size (`MaxLS`).}
#'   \item{SAM}{The size adherence multiplier. When SAM > 1, catch advice increases, When SAM < 1, catch advice decreases.}
#' }
#'
#' @examples
#' L_2.5 <- 15     # 2.5th percentile
#' L_97.5 <- 35    # 97.5th percentile
#' MinLS <- 20     # Minimum landing size
#' MaxLS <- 40     # Maximum landing size
#'
#' SAM(L_2.5, L_97.5, MinLS, MaxLS)
#'
#' @seealso \code{\link{percentile}} for calculating `L_2.5` and `L_97.5` from length-frequency data.
#'
#' @export
SAM <- function(L_2.5, L_97.5, MinLS, MaxLS) {
  # Validate inputs
  if (!is.numeric(L_2.5) || !is.numeric(L_97.5) || !is.numeric(MinLS) || !is.numeric(MaxLS)) {
    stop("All inputs must be numeric.")
  }

  # Calculate the relative deviations from MinLS and MaxLS
  lower_adherence <- (L_2.5 - MinLS) / MinLS
  upper_adherence <- (MaxLS - L_97.5) / MaxLS

  # Calculate the multiplier
  if (L_2.5 >= MinLS & L_97.5 <= MaxLS) {
    # Case 1: No slot limit violation
    multiplier <- (1 + lower_adherence) * (1 + upper_adherence)
  } else {
    # Case 2: Violation of either slot limit
    multiplier <- (1 + lower_adherence) * (1 + upper_adherence)
    multiplier <- min(1, multiplier)  # Cap at 1
  }

  # Return the results as a list
  return(list(
    lower_adherence = round(lower_adherence, 2),
    upper_adherence = round(upper_adherence, 2),
    SAM = round(multiplier, 2)))
}
