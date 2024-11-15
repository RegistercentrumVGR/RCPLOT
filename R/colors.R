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
#'
#' @examples
#' colors_rc(2)
colors_rc <- function(n = 3, type = "default") {
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
