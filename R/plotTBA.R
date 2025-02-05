#' plotTBA
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 geom_hline
#' @importFrom ggplot2 geom_point labs coord_cartesian theme_bw theme
#' @importFrom ggplot2 element_text
#' @importFrom grid unit
#'
#' @description
#' This function produces a plot visualizing the behavior of the Targeted Biomass Adjustment (TBA) equation, upon which the input data are plotted.
#' The plot can be saved as an object and further customized using `ggplot2` functions.
#'
#' @param P_targeted The proportion of the size range targeted by harvest slot limits, calculated using the `prop_target` function.
#' @param rB The proportional rate of change in a biomass index, calculated using the `rB` function.
#'
#' @return A `ggplot2` plot object
#'
#' @examples
#' P_targeted <- 0.5
#' rB <- -0.5
#' p <- plotTBA(P_targeted, rB)
#' p + ggplot2::theme(text = ggplot2::element_text(size = 20))
#'
#' @seealso \code{\link{prop_target}} # for the proportion targeted by HSL
#' @seealso \code{\link{rB}} # for the proportional rate of change in a biomass
#' @seealso \code{\link{TBA}} # for TBA function
#'
#' @export
plotTBA <- function(P_targeted, rB) {

  # Input validation
  if (!is.numeric(P_targeted) || P_targeted < 0 || P_targeted > 1) {
    stop("P_targeted must be a numeric value between 0 and 1.")
  }
  if (!is.numeric(rB) || rB < -1 || rB > 1) {
    stop("rB must be a numeric value between -1 and 1.")
  }

  # Create ranges for P_targeted and rB
  P_targeted_vals <- seq(0, 1, by = 0.01)
  rB_vals <- seq(-1, 1, by = 0.01)

  # Create a grid of P_targeted and rB combinations
  grid <- expand.grid(P_targeted_vals = P_targeted_vals, rB_vals = rB_vals)

  # Calculate constrained multiplier for each combination
  grid$Multiplier <- mapply(TBA, grid$P_targeted_vals, grid$rB_vals)

  # Generate the plot
  ggplot2::ggplot(grid, ggplot2::aes(x = P_targeted_vals, y = rB_vals, fill = Multiplier)) +
    ggplot2::geom_tile(alpha=0.85) +
    ggplot2::scale_fill_gradient2(
      name = "",
      low = "darkred", mid = "lightyellow", high = "darkgreen", midpoint = 1,
      limits = c(1 - T, 1 + T)
    ) +
    ggplot2::geom_hline(yintercept = 0, color = "black", linetype = "dashed", linewidth = 0.25) +
    ggplot2::geom_point(
      ggplot2::aes(x = P_targeted, y = rB),
      color = "black", size = 3
    ) +
    ggplot2::labs(
      title = "Targeted Biomass Adjustment",
      x = "Proportion Targeted",
      y = "Proportional change in Biomass",
      fill = "Multiplier"
    ) +
    ggplot2::coord_cartesian(xlim = c(0, 1), ylim = c(-1, 1), expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
                   text = ggplot2::element_text(size = 20))
}
