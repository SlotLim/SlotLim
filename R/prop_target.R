#' prop_target
#'
#' @importFrom ggplot2 ggplot aes geom_line geom_vline geom_ribbon annotate labs theme_bw theme coord_cartesian
#' @importFrom grid unit
#' @importFrom stats integrate
#' @importFrom utils capture.output head
#'
#' @description
#' The function estimates the proportion of abundance targeted by harvest slot limits from a length-structured survivorship curve. If left blank, M is calculated using t_max as per Hoenig's equation modified by Then et al. (2015): Then, A. Y., Hoenig, J. M., Hall, N. G., Hewitt, D. A. & Handling editor: Ernesto Jardim. (2015). Evaluating the predictive performance of empirical estimators of natural mortality rate using information on over 200 fish species. ICES Journal of Marine Science 72, 82 – 92.
#'
#' @param MinLS The minimum landing size.
#' @param MaxLS The maximum landing size.
#' @param M Instantaneous natural mortality rate.
#' @param L_inf Asymptotic length.
#' @param K Juvenile growth coefficient.
#' @param t_0 Hypothetical age at which length is zero.
#' @param t_max Maximum age.
#' @param L_min To account for non-zero length-at-age-0, survivorship is modeled from 1*10^-5.
#'
#' @return The proportion of abundance targeted by harvest slot limits.
#'
#' @examples
#' MinLS <- 120
#' MaxLS <- 240
#' M <- 0.19
#' L_inf <- 405
#' K <- 0.118
#' t_0 <- -1.83
#' t_max <- 34
#' prop_target(MinLS, MaxLS, M, L_inf, K, t_0, t_max)
#'
#' @export
prop_target <- function(MinLS, MaxLS, M = NULL, L_inf, K, t_0, t_max, L_min = 1e-5) {
  # If M is not specified, calculate it using the formula
  if (is.null(M)) {
    M <- 4.899 * t_max^-0.916
  }

  # Function to calculate age from length using the inverse von Bertalanffy equation
  age_from_length <- function(L) {
    age <- (log((L_inf - L) / L_inf)) / (-K) + t_0
    age[L >= L_inf] <- t_max  # If length exceeds L_inf, return t_max for those values
    age[L <= 0] <- 0  # If length is 0 or negative, return 0
    return(age)
  }

  # Calculate length corresponding to t_max using von Bertalanffy model
  length_at_t_max <- L_inf * (1 - exp(-K * (t_max - t_0)))

  # Define the integrand for survivorship (decaying exponentially with age)
  integrand <- function(L) {
    age <- age_from_length(L)
    return(exp(-M * age))  # Survivorship at age
  }

  # Create a dataframe to store length and survivorship values
  length_values <- seq(L_min, L_inf, by = 0.1)  # Start from L_min instead of 0
  survivorship_values <- sapply(length_values, integrand)

  # Normalize survivorship so that S(0) = 1 (this ensures survivorship is <= 1 at all ages)
  S0 <- survivorship_values[length_values == L_min]  # Survivorship at the minimum length
  normalized_survivorship_values <- survivorship_values / S0  # Normalize to ensure S(0) = 1

  # Create a dataframe for easier inspection
  survivorship_df <- data.frame(
    Length = length_values,
    Survivorship = normalized_survivorship_values
  )

  # Debugging: Print the first few rows to check if survivorship starts at 1
  print(head(survivorship_df))

  # Integrate total survivorship over the full range (L_min to L_inf)
  total_survivorship <- integrate(function(L) exp(-M * age_from_length(L)), lower = L_min, upper = L_inf)$value

  # Integrate survivorship within the specified range (MinLS to MaxLS)
  range_survivorship <- integrate(function(L) exp(-M * age_from_length(L)), lower = MinLS, upper = MaxLS)$value

  # Calculate the proportion of individuals between MinLS and MaxLS
  proportion <- range_survivorship / total_survivorship

  # Debug information for checking
  # Print the value of M
  cat("M =", round(M, 2), "\n")  # Print M value
  cat("t_min @ MinLS:", round(age_from_length(MinLS),2), "\n")
  cat("t_max @ MaxLS:", round(age_from_length(MaxLS),2), "\n")
  cat("Total survivorship (denominator):", round(total_survivorship,2), "\n")
  cat("Range survivorship (numerator):", round(range_survivorship,2), "\n")
  cat("Proportion:", round(proportion,2), "\n")

  # Plot the survivorship curve with filled area under the curve and custom limits
  p <- ggplot2::ggplot(survivorship_df, ggplot2::aes(x = Length, y = Survivorship)) +
    ggplot2::geom_line(color = "darkgreen", size = 1) +    # Optional: Add the line on top
    ggplot2::geom_vline(xintercept = MinLS, color = "darkred", linetype = "dashed", linewidth = 1.5) +
    ggplot2::geom_vline(xintercept = MaxLS, color = "darkred", linetype = "dashed", linewidth = 1.25) +
    ggplot2::geom_ribbon(data = subset(survivorship_df, Length >= MinLS & Length <= MaxLS),
                         ggplot2::aes(x = Length, ymin = 0, ymax = Survivorship), fill = "yellow", alpha = 0.3) + # Light yellow fill
    ggplot2::annotate("text", x = MinLS * 1.1, y = 0.9, label = "Min LS", color = "black", angle = 90, size = 5) +
    ggplot2::annotate("text", x = MaxLS * 0.95, y = 0.9, label = "Max LS", color = "black", angle = 90, size = 5) +
    ggplot2::labs(title = "Proportion targeted",
                  x = "Length", y = "Survivorship") +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
                   text = ggplot2::element_text(size = 20)) +
    ggplot2::coord_cartesian(ylim = c(0, 1), xlim = c(0, length_at_t_max), expand = FALSE)  # Set axis limits

  print(p)

  # Return the proportion
  return(round(proportion, 2))
}
