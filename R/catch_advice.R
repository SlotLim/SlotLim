#' catch_advice
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 geom_vline annotate geom_hline labs coord_cartesian theme_bw theme
#'
#' @description
#' The function calculates catch advice using the SlotLim framework.
#'
#' @param Cy The most recent annual catch, or average catch from multiple years.
#' @param TBA The Targeted Biomass Adjustment calculated using the `TBA` function.
#' @param SAM The Size Adherence Multiplier calculated using the `SAM` function.
#' @param T Threshold for limiting changes to catch advice. Default is `0.2` (i.e., +/- 20%).
#'
#' @return Annual catch advice. Units are equal to those input for Cy.
#'
#' @examples
#' Cy <- 1000
#' TBA <- 1.1
#' SAM <- 0.9
#' T <- 0.2
#'
#' Ay <- catch_advice(Cy, TBA, SAM, T)[[1]]
#'
#' @seealso \code{\link{TBA}} # for the Targeted Biomass Adjustment
#' @seealso \code{\link{SAM}} # for the Size Adherence Multiplier
#'
#' @export
catch_advice <- function(Cy, TBA, SAM, T = 0.2) {
  # Input validation
  if (!is.numeric(Cy) || Cy <= 0) stop("Cy must be a positive numeric value.")
  if (!is.numeric(TBA) || TBA <= 0) stop("TBA must be a positive numeric value.")
  if (!is.numeric(SAM) || SAM <= 0) stop("SAM must be a positive numeric value.")
  if (!is.numeric(T) || T <= 0 || T >= 1) stop("T must be a numeric value between 0 and 1.")

  # Calculate Ay
  Ay <- min((1 + T) * Cy, max((1 - T) * Cy, Cy * TBA * SAM))

  # Calculate Ay_percent as the percentage change from Cy
  Ay_percent <- ((Ay - Cy) / Cy) * 100

  # Generate TBA and SAM values dynamically
  TBA_vals <- seq(1 - 0.99, 1 + 0.99, length.out = 100)
  SAM_vals <- seq(1 - 0.99, 1 + 0.99, length.out = 100)

  # Create a grid of TBA and SAM values
  grid <- expand.grid(TBA_vals = TBA_vals, SAM_vals = SAM_vals)

  # Calculate percentage deviation from Cy for each combination
  grid$Deviation <- with(
    grid,
    (pmin((1 + T) * Cy, pmax((1 - T) * Cy, Cy * TBA_vals * SAM_vals)) - Cy) / Cy * 100
  )

  # Dynamically adjust axis limits based on the grid and data point
  x_min <- min(c(grid$TBA_vals, TBA)) * 0.95
  x_max <- max(c(grid$TBA_vals, TBA)) * 1.05
  y_min <- min(c(grid$SAM_vals, SAM)) * 0.95
  y_max <- max(c(grid$SAM_vals, SAM)) * 1.05

  # Create the plot without contouring
  plot <- ggplot2::ggplot(grid, ggplot2::aes(x = TBA_vals, y = SAM_vals, fill = Deviation)) +
    ggplot2::geom_tile(alpha=0.85) +
    ggplot2::scale_fill_gradient2(
      name = "",
      low = "darkred", mid = "lightyellow", high = "darkgreen", midpoint = 0,
      limits = c(min(grid$Deviation), max(grid$Deviation))
    ) +
    ggplot2::geom_point(ggplot2::aes(x = TBA, y = SAM), color = "black", size = 3) +
    ggplot2::labs(
      title = "Catch Advice (%)",
      x = "Targeted Biomass Adjustment",
      y = "Size Adherence Multiplier",
      fill = ""
    ) +
    ggplot2::coord_cartesian(xlim = c(min(TBA-0.1, 1-T), max(TBA+0.1, 1+T)), ylim = c(min(SAM-0.1, 1-T), max(SAM+0.1, 1+T)), expand = FALSE) +
    ggplot2::theme_bw() +
    ggplot2::theme(text = ggplot2::element_text(size = 15))

  return(list(Ay = Ay,
              Ay_percent = Ay_percent,
              plot = plot))
}
