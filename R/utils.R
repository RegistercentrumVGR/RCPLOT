#' ggsave function with default settings as specified with options()
#'
#' @param file File name to create on disk.
#' @param plot Plot to save
#' @param width Width in units ("in", "cm", or "mm").
#'              default: options("figure_width")
#' @param height Height in units ("in", "cm", or "mm").
#' @param units Units ("in", "cm", or "mm")
#' @param dpi Plot resolution.
#' @param device Device to use. Can either be a device function (e.g. png()), or one of "eps", "ps", "tex" (pictex), "pdf", "jpeg", "tiff", "png", "bmp", "svg" or "wmf" (windows only).
#' @param family Font family
#' @param ... additional arguments passed to `ggplot2::ggsave` and
#'            then to the selected `device`.
#' @export
#' @import ggplot2
#'
rcsave <- function(
  file,
  plot = ggplot2::last_plot(),
  width = getOption("figure_width"),
  height = getOption("figure_height"),
  units = "in",
  dpi = getOption("figure_dpi"),
  device = getOption("figure_device"),
  family = getOption("figure_family"),
  ...
) {

  if(device == "tiff"){
    ggplot2::ggsave(
      filename = file,
      plot = plot,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      type = "cairo",
      device = device,
      family = family,
      compression = "lzw",
      ...
    )
  }else if(device == "pdf"){
    ggplot2::ggsave(
      filename = file,
      plot = plot,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      device = device,
      ...
    )
  }else{
    ggplot2::ggsave(
      filename = file,
      plot = plot,
      width = width,
      height = height,
      units = units,
      dpi = dpi,
      type = "cairo",
      device = device,
      family = family,
      ...
    )
  }

}
