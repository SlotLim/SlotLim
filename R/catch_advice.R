#' catch_advice
#'
#' @importFrom ggplot2 ggplot aes geom_tile scale_fill_gradient2 geom_point labs
#' @importFrom ggplot2 coord_cartesian theme_bw theme element_text
#' @importFrom grid unit
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
catch_advice <- function(Cy, TBA, SAM, T = NULL) {
  # Input validation
  if (!is.numeric(Cy) || Cy <= 0) stop("Cy must be a positive numeric value.")
  if (!is.numeric(TBA) || TBA <= 0) stop("TBA must be a positive numeric value.")
  if (!is.numeric(SAM) || SAM <= 0) stop("SAM must be a positive numeric value.")
  if (!is.null(T) && (!is.numeric(T) || T <= 0 || T >= 1)) stop("T must be a numeric value between 0 and 1.")

  # Calculate Ay correctly
  if (is.null(T)) {
    Ay <- Cy * TBA * SAM  # No cap when T is NULL
  } else {
    Ay <- min((1 + T) * Cy, max((1 - T) * Cy, Cy * TBA * SAM))  # Cap when T is provided
  }

  # Calculate Ay_percent as the percentage change from Cy
  Ay_percent <- ((Ay - Cy) / Cy) * 100

  # Generate TBA and SAM values dynamically
  TBA_vals <- seq(1 - 0.99, 1 + 0.99, length.out = 100)
  SAM_vals <- seq(1 - 0.99, 1 + 0.99, length.out = 100)

  # Create a grid of TBA and SAM values
  grid <- expand.grid(TBA_vals = TBA_vals, SAM_vals = SAM_vals)

  # Correctly calculate deviation for the color scale
  if (is.null(T)) {
    grid$Deviation <- ((Cy * grid$TBA_vals * grid$SAM_vals) - Cy) / Cy * 100  # No cap
    color_limits <- c(-100, 100)  # Fixed color scale when T is NULL
  } else {
    grid$Deviation <- ((pmin((1 + T) * Cy, pmax((1 - T) * Cy, Cy * grid$TBA_vals * grid$SAM_vals))) - Cy) / Cy * 100
    color_limits <- c(-T * 100, T * 100)  # Color scale based on T
  }

  # Ensure color limits are properly applied
  grid$Deviation <- pmax(pmin(grid$Deviation, max(color_limits)), min(color_limits))

  # Dynamically adjust axis limits
  x_min <- min(c(grid$TBA_vals, TBA)) * 0.95
  x_max <- max(c(grid$TBA_vals, TBA)) * 1.05
  y_min <- min(c(grid$SAM_vals, SAM)) * 0.95
  y_max <- max(c(grid$SAM_vals, SAM)) * 1.05

  # Create the plot
  plot <- ggplot2::ggplot(grid, ggplot2::aes(x = TBA_vals, y = SAM_vals, fill = Deviation)) +
    ggplot2::geom_tile(alpha = 0.85) +
    ggplot2::scale_fill_gradient2(
      name = "",
      low = "darkred", mid = "lightyellow", high = "darkgreen", midpoint = 0,
      limits = color_limits
    ) +
    ggplot2::geom_point(ggplot2::aes(x = TBA, y = SAM), color = "black", size = 3) +
    ggplot2::labs(
      title = "Catch Advice (%)",
      x = "Targeted Biomass Adjustment",
      y = "Size Adherence Multiplier"
    ) +
    ggplot2::coord_cartesian(
      xlim = c(min(TBA - 0.1, ifelse(is.null(T), 1 - 0.2, 1 - T)), max(TBA + 0.1, ifelse(is.null(T), 1 + 0.2, 1 + T))),
      ylim = c(min(SAM - 0.1, ifelse(is.null(T), 1 - 0.2, 1 - T)), max(SAM + 0.1, ifelse(is.null(T), 1 + 0.2, 1 + T))),
      expand = FALSE
    ) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      plot.margin = grid::unit(c(1, 1, 1, 1), "cm"),
      text = ggplot2::element_text(size = 20)
    )

  return(list(Ay = Ay, Ay_percent = Ay_percent, plot = plot))
}
