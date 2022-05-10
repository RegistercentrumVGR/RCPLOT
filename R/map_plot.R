#' Plot map using geom_sf
#'
#' @param df data.frame map data. One column
#'           should be named "geometry" containing map
#'           coordinates for regions to map.
#' @param fill_var Variable to use for colouring regions
#' @param label_var Variable to use label names
#' @param fill_colors Manually set colors
#' @param legend_labels Manually set labels, character vector.
#' @param label_breaks Manually set legend breaks
#' @param legend_row Number of rows in legend
#' @param legend_col Number of cols in legend
#'
#' @return ggplot object with map figure
#' @example man/examples/map_plot.R
#' @import sf
#' @export

map_plot <- function(
  df,
  fill_var      = "id",
  label_var     = NULL,
  fill_colors   = NULL,
  legend_labels = ggplot2::waiver(),
  label_breaks  = ggplot2::waiver(),
  legend_row    = NULL,
  legend_col    = NULL
  ){

  # Fill colors ------------------------------------------------------------
  if (is.null(fill_colors)) {
    n <- if (!is.null(fill_var)) length(unique(df[[fill_var]])) else NULL
    fill_colors <- colors_select(n)
  }

  # Create map plot
  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = df,
      mapping = aes(fill = .data[[fill_var]])
    ) +
    ggplot2::scale_fill_manual(
      values = fill_colors,
      labels = legend_labels,
      breaks = label_breaks,
      guide  = ggplot2::guide_legend(nrow = legend_row, ncol = legend_col)
    ) +
    ggplot2::theme_void()


  if(!is.null(label_var)){
    # Add region names
    p <- p +
      ggplot2::geom_sf_label(
        data = df,
        mapping = ggplot2::aes(label = .data[[label_var]]),
        label.size = 0.01,
        label.padding = unit(0.15, "lines")
      )
  }

  return(p)
}
