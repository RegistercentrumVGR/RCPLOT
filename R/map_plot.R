#' Plot map using geom_sf
#'
#' @param df data.frame map data. One column
#'           should be named "geometry" containing map
#'           coordinates for regions to map.
#' @param fill_var Variable to use for colouring regions
#' @param label_var Variable to use label names
#'
#' @return ggplot object with map figure
#' @example man/examples/map_plot.R
#' @import sf
#' @export

map_plot <- function(
  df,
  fill_var = "id",
  label_var = NULL
  ){

  p <- ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = df,
      mapping = aes(fill = .data[[fill_var]])
    ) +
    ggplot2::theme_void()


  if(!is.null(label_var)){
    # Add region names
    p <- p +
      ggplot2::geom_sf_label(
        data = df,
        mapping = ggplot2::aes(label = .data[[label_var]]),
        position = position_dodge(width = 2)
      )
  }

  return(p)
}
