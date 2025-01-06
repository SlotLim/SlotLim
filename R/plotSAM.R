#' plotSAM
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 geom_vline annotate geom_hline labs coord_cartesian theme_bw theme
#' @importFrom utils capture.output
#'
#' @description
#' This function produces a plot visualizing the behavior of the Size Adherence Multiplier (SAM) equation, upon which the input data are plotted.
#' The plot can be saved as an object and further customized using `ggplot2` functions. Recommended input units are cm. For larger species, reduce the resolution to speed up plotting.
#'
#' @param L_5 The 5th percentile of the catch from fishery-dependent length-frequency data.
#' @param L_95 The 95th percentile of the catch from fishery-dependent length-frequency data.
#' @param MinLS The minimum landing size.
#' @param MaxLS The maximum landing size.
#' @param res Resolution for generating observations. Default is `1`. Smaller resolution values increase smoothness but may result in longer computation times.
#'
#' @return A `ggplot2` plot object
#'
#' @examples
#' L_5 <- 35       # 5th percentile
#' L_95 <- 50      # 95th percentile
#' MinLS <- 30     # Minimum landing size
#' MaxLS <- 50     # Maximum landing size
#' res <- 0.5        # Resolution
#'
#' p <- plotSAM(L_5, L_95, MinLS, MaxLS, res)
#' p + ggplot2::theme(text = ggplot2::element_text(size = 10))
#'
#' @seealso \code{\link{SAM}} # For the Size Adherence Multiplier
#'
#' @export
plotSAM <- function(L_5, L_95, MinLS, MaxLS, res = 1) {

  # Input validation
  if (!is.numeric(L_5) || !is.numeric(L_95) || !is.numeric(MinLS) || !is.numeric(MaxLS) || !is.numeric(res)) {
    stop("All inputs must be numeric.")
  }
  if (any(L_5 < 1, L_95 < 1, MinLS < 1, MaxLS < 1)) {
    stop("Inputs must be > 1, recommended units are cm")
  }
  if (res <= 0) {
    stop("Resolution (res) must be greater than 0.")
  }

  # Define ranges for L_5 and L_95
  L_5_range <- seq(0.1 * MinLS, 1.9 * MinLS, by = res)  # Varying L_5
  L_95_range <- seq(0.1 * MinLS, 1.9 * MaxLS, by = res) # Varying L_95

  # Create a data frame to store the results
  results <- expand.grid(compL_5 = L_5_range, compL_95 = L_95_range)

  # Suppress the output from the SAM function by redirecting the output
  suppressOutput <- function(expr) {
    capture.output(expr, file = NULL)  # Suppresses output to console
  }

  # Apply the SAM function to each combination of L_5 and L_95
  results$SAM <- sapply(1:nrow(results), function(i) {
    suppressOutput({
      sam_result <- SAM(results$compL_5[i], results$compL_95[i], MinLS, MaxLS)
      return(sam_result$SAM)  # Extract just the SAM value from the result
    })
  })

  # Create the plot
  plot <- ggplot2::ggplot(results, ggplot2::aes(x = compL_5, y = compL_95, fill = SAM)) +
    ggplot2::geom_tile(alpha=0.85) +
    ggplot2::scale_fill_gradient2(
      name = "",
      low = "darkred", mid = "lightyellow", high = "darkgreen", midpoint = 1,
      limits = c(1 - T, 1 + T)
    ) +
    # Min landing size
    ggplot2::geom_vline(xintercept = MinLS, color = "black", linetype = "dashed", linewidth = 0.5) +
    ggplot2::annotate("text", x = MinLS*0.95, y = 0.1*max(L_95_range), label = "Min LS", color = "black", angle = 90, size = 4) +
    # Max landing size
    ggplot2::geom_hline(yintercept = MaxLS, color = "black", linetype = "dashed", linewidth = 0.5) +
    ggplot2::annotate("text", x = 0.9*max(L_5_range), y = MaxLS*1.05, label = "Max LS", color = "black", size = 4) +
    # Input data
    ggplot2::geom_point(ggplot2::aes(x = L_5, y = L_95), color = "black", size = 3) +
    # Tidy plot
    ggplot2::labs(
      title = "Size Adherence Multiplier",
      x = expression(L[5]),
      y = expression(L[95]),
      fill = "SAM"
    ) +
    ggplot2::coord_cartesian(xlim = range(L_5_range), ylim = range(L_95_range), expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 15))

  return(plot)
}
