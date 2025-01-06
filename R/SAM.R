#' SAM
#'
#' @description
#' This function calculates the Size Adherence Multiplier (SAM), which evaluates the adherence of the catch distribution to harvest slot limits.
#' When `L_5 >= MinLS` and `L_95 <= MaxLS`, neither harvest slot limits are violated and the multiplier is calculated without constraint.
#' When `L_5 < MinLS` or `L_95 > MaxLS`, harvest slot limits have been violated and the multiplier is constrained to a maximum of `1`.
#'
#' @param L_5 The 5th percentile of the catch from fishery-dependent length-frequency data.
#' @param L_95 The 95th percentile of the catch from fishery-dependent length-frequency data.
#' @param MinLS The minimum landing size.
#' @param MaxLS The maximum landing size.
#'
#' @return A list containing the following elements:
#' \describe{
#'   \item{lower_adherence}{The relative deviation of the 5th percentile (`L_5`) from the minimum landing size (`MinLS`).}
#'   \item{upper_adherence}{The relative deviation of the 95th percentile (`L_95`) from the maximum landing size (`MaxLS`).}
#'   \item{SAM}{The size adherence multiplier. When SAM > 1, catch advice increases, When SAM < 1, catch advice decreases.}
#' }
#'
#' @examples
#' L_5 <- 15       # 5th percentile
#' L_95 <- 35      # 95th percentile
#' MinLS <- 20     # Minimum landing size
#' MaxLS <- 40     # Maximum landing size
#'
#' SAM(L_5, L_95, MinLS, MaxLS)
#'
#' @seealso \code{\link{percentile}} for calculating `L_5` and `L_95` from length-frequency data.
#'
#' @export
SAM <- function(L_5, L_95, MinLS, MaxLS) {
  # Validate inputs
  if (!is.numeric(L_5) || !is.numeric(L_95) || !is.numeric(MinLS) || !is.numeric(MaxLS)) {
    stop("All inputs must be numeric.")
  }

  # Calculate the relative deviations from MinLS and MaxLS
  lower_adherence <- (L_5 - MinLS) / MinLS
  upper_adherence <- (MaxLS - L_95) / MaxLS

  # Calculate the multiplier
  if (L_5 >= MinLS & L_95 <= MaxLS) {
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
