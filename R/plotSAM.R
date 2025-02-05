#' plotSAM
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 geom_vline
#' @importFrom ggplot2 annotate geom_hline geom_point labs coord_cartesian theme_bw theme
#' @importFrom ggplot2 element_text
#' @importFrom grid unit
#' @importFrom stats quantile
#'
#' @description
#' This function produces a plot visualizing the behavior of the Size Adherence Multiplier (SAM) equation, upon which the input data are plotted.
#' The plot can be saved as an object and further customized using `ggplot2` functions. Recommended input units are cm. For larger species, reduce the resolution to speed up plotting.
#'
#' @param L_2.5 The 2.5th percentile of the catch from fishery-dependent length-frequency data.
#' @param L_97.5 The 97.5th percentile of the catch from fishery-dependent length-frequency data.
#' @param MinLS The minimum landing size.
#' @param MaxLS The maximum landing size.
#' @param res Resolution for generating observations. Default is `1`. Smaller resolution values increase smoothness but may result in longer computation times.
#'
#' @return A `ggplot2` plot object
#'
#' @examples
#' L_2.5 <- 35      # 2.5th percentile
#' L_97.5 <- 50     # 97.5th percentile
#' MinLS <- 30      # Minimum landing size
#' MaxLS <- 50      # Maximum landing size
#' res <- 0.5       # Resolution
#'
#' p <- plotSAM(L_2.5, L_97.5, MinLS, MaxLS, res)
#' p + ggplot2::theme(text = ggplot2::element_text(size = 10))
#'
#' @seealso \code{\link{SAM}} # For the Size Adherence Multiplier
#'
#' @export
plotSAM <- function(L_2.5, L_97.5, MinLS, MaxLS, res = 1) {

  # Input validation
  if (!is.numeric(L_2.5) || !is.numeric(L_97.5) || !is.numeric(MinLS) || !is.numeric(MaxLS) || !is.numeric(res)) {
    stop("All inputs must be numeric.")
  }
  if (any(L_2.5 < 1, L_97.5 < 1, MinLS < 1, MaxLS < 1)) {
    stop("Inputs must be > 1, recommended units are cm")
  }
  if (res <= 0) {
    stop("Resolution (res) must be greater than 0.")
  }

  # Define ranges for L_5 and L_95
  L_2.5_range <- seq(0.1 * MinLS, 1.9 * MinLS, by = res)  # Varying L_5
  L_97.5_range <- seq(0.1 * MinLS, 1.9 * MaxLS, by = res) # Varying L_95

  # Create a data frame to store the results
  results <- expand.grid(compL_2.5 = L_2.5_range, compL_97.5 = L_97.5_range)

  # Suppress the output from the SAM function by redirecting the output
  suppressOutput <- function(expr) {
    capture.output(expr, file = NULL)  # Suppresses output to console
  }

  # Apply the SAM function to each combination of L_2.5 and L_97.5
  results$SAM <- sapply(1:nrow(results), function(i) {
    suppressOutput({
      sam_result <- SAM(results$compL_2.5[i], results$compL_97.5[i], MinLS, MaxLS)
      return(sam_result$SAM)  # Extract just the SAM value from the result
    })
  })

  # Create the plot
  plot <- ggplot2::ggplot(results, ggplot2::aes(x = compL_2.5, y = compL_97.5, fill = SAM)) +
    ggplot2::geom_tile(alpha=0.85) +
    ggplot2::scale_fill_gradient2(
      name = "",
      low = "darkred", mid = "lightyellow", high = "darkgreen", midpoint = 1,
      limits = c(1 - T, 1 + T)
    ) +
    # Min landing size
    ggplot2::geom_vline(xintercept = MinLS, color = "black", linetype = "dashed", linewidth = 0.5) +
    ggplot2::annotate("text", x = MinLS*0.95, y = 0.1*max(L_97.5_range), label = "Min LS", color = "black", angle = 90, size = 4) +
    # Max landing size
    ggplot2::geom_hline(yintercept = MaxLS, color = "black", linetype = "dashed", linewidth = 0.5) +
    ggplot2::annotate("text", x = 0.9*max(L_2.5_range), y = MaxLS*1.05, label = "Max LS", color = "black", size = 4) +
    # Input data
    ggplot2::geom_point(ggplot2::aes(x = L_2.5, y = L_97.5), color = "black", size = 3) +
    # Tidy plot
    ggplot2::labs(
      title = "Size Adherence Multiplier",
      x = expression(L[2.5]),
      y = expression(L[97.5]),
      fill = "SAM"
    ) +
    ggplot2::coord_cartesian(xlim = range(L_2.5_range), ylim = range(L_97.5_range), expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
                   text = ggplot2::element_text(size = 20))

  return(plot)
}
