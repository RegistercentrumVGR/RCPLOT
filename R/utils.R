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

#' Add attributes from one data frame to another
#'
#' Add "map" attributes from df2 to df1.
#' @param df1 Data frame to receive attributes.
#' @param df2 Data frame to give attributes.
#' @return Data frame.
#' @example man/examples/add_attr.R
#' @export
add_attr <- function(df1, df2) {
  for (i in names(df1)) {
    attr(df1[[i]], "map") <- attr(df2[[i]], "map")
  }
  df1
}

#' Convert object with attributes in "map" to factor
#'
#' Used to extract texts from a variable containing numeric values.
#' @param x Object with "map" attributes, where levels and labels exist
#' @return Factor
#' @example man/examples/attr_to_factor.R
#' @export
attr_to_factor <- function(x) {
  factor(x, attr(x, "map")$levels, attr(x, "map")$labels)
}

#' Check if object has attribute "map"
#'
#' Useful with [dplyr::mutate_if()]
#' @param x Vector
#' @return `TRUE` or `FALSE`
#' @example man/examples/check_attr.R
#' @export
check_attr <- function(x){
  !is.null(attr(x, "map"))
}

#' Help function for add_prom
#'
#' If there are several PROM on same date, this function populate rows with
#' existing PROM values.
#'
#' @param x vector wit hPROM values
pick_valid <- function(x) {
  x <- unique(x)
  y <- x[!is.na(x)]

  if (length(y) > 1) {
    print("This patient has several PROM-values, one is chosen randomly:", y)
    sample(y, 1)
  } else if (length(x) > 1) {
    y
  } else {
    x
  }
}

#' Make an ordered factor with most important level first
#'
#' Useful when making bar plots and have the most common level in the bottom.
#' Also possible to remove uncommon levels and create new level called other.
#' @param x Vector
#' @param other_count How many levels should be considered as other level
#' @param other_level What the new level should be called
#' @example man/examples/size_order.R
#' @export
size_order <- function(x, other_count = NULL, other_level = "\u00D6vriga") {
  ordning <- names(sort(table(x), decreasing = TRUE))
  x <- ordered(x, levels = ordning)

  if (!is.null(other_count)) {
    n_levels <- length(levels(x)) - other_count
    levels(x) <- c(levels(x)[1:n_levels], rep(other_level, other_count))
  }
  x
}
