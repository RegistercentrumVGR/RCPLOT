#' Wrapper for RColorBrewer to get colors
#'
#' @param n number of colors
#' @param type one of
#'       `"default"`, for standard colors.
#'       `"RYG"`, for red-yellow-green scale.
#'       `"Paired"` for pairwise colors.
#'
#' @return vector with color codes
#' @export
colors_rc <- function(n = 3, type = "default") {
  lifecycle::deprecate_soft(
    when = "1.1.0", what = "colors_rc()", with = "colors_rc_2()"
  )
  color_names <- list(
    default = "Set3",
    RYG = "RdYlGn",
    Paired = "Paired"
  )
  col <- RColorBrewer::brewer.pal(
    max(n, 3), color_names[[type]]
  )[1:max(n, 1)]
  return(col)
}

#' Registercentrum standard colors
#'
#' Get a number of colors according to Registercentrum standards. Three palettes
#' are available based on the type of palette desired.
#'
#' A qualitative palette should be used when we have groups that are all equal
#' in value, for example some different type of treatment. A sequential palette
#' should be used when data has an inherent order, for example age groups, pain
#' scale, etc. Finally, a diverging palette should be used when data is centered
#' around a neutral value, for example data measuring improvement/deteriation
#' after a certain treatment.
#'
#' The qualitative palette is Okabe-Ito, the sequential is viridis, and the
#' divering is Purple-Green.
#'
#' @param n the number of colors desired
#' @param type the type of palette, one of "qualitative", "sequential", and
#' "diverging"
#'
#' @export
colors_rc_2 <- function(n, type = "qualitative") {

  checkmate::assert_integerish(n, lower = 1, len = 1, any.missing = FALSE)
  checkmate::assert_choice(type, c("qualitative", "sequential", "diverging"))

  col <- switch(
    type,
    qualitative = grDevices::palette.colors(
      n = 9, palette = "Okabe-Ito"
    )[c(6:8, 3:5, 1:2, 9)][1:n],
    sequential = grDevices::hcl.colors(n = n, palette = "viridis"),
    diverging = grDevices::hcl.colors(n = n, palette = "Blue-Red 2")
  )

  col <- unname(col)

  return(col)

}
