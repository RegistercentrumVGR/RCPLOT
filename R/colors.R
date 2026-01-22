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
  lifecycle::deprecate_soft(
    when = "1.6.0", what = "colors_rc_2()", with = "colors_rc_3()"
  )
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

#' Registercentrum standard colors
#'
#' Color palettes used as a standard in Registercentrum.
#' The colors are based on input from UX-designers and have been chosen to
#' accommodate levels of visual impairment.
#'
#' There are 3 color palettes, categorical (used for grouped data in
#' which the categories are not supposed to be graded) and two sequential
#' palletes.
#'
#'
#' @param n the number of colors desired
#' @param type the type of palette, one of "qualitative", "sequential_1", and
#' "sequential_2"
#'
#' @export
colors_rc_3 <- function(n, type = "qualitative") {

  checkmate::assert_integerish(n, lower = 1, len = 1, any.missing = FALSE)
  checkmate::assert_choice(type, c("qualitative",
                                   "sequential_1",
                                   "sequential_2"))

  rc_colors <- list(
    qualitative = c("#116875", "#FC5930", "#6F45BB",
                    "#89163B", "#1A9FB3", "#E02460",
                    "#3A2168", "#A48C83", "#051F23",
                    "#9575CD", "#AC641C", "#6D3912"),
    sequential_1 = c("#37863A", "#AC641C", "#E63C37"),
    sequential_2 = c("#1A9FB3", "#158192", "#116875",
                     "#0D4E58", "#09363C", "#051F23")
  )

  rc_colors[[type]][seq_len(min(n, length(rc_colors[[type]])))]

}
