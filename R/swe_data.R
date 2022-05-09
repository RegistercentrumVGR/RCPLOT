#' Map coordinates for Sweden counties
#'
#' @format A sf dataset with one row per county
#' \describe{
#'   \item{id}{ID of county (as in data from SCB)}
#'   \item{geometry}{Coordinate polygon used to draw county borders}
#'   \item{Name}{Name of county}
#' }
#'
#' @example man/examples/map_plot.R
#'
#' @source
#'   \url{http://api.thenmap.net/v2/se-4/geo/2020-06-03}
#'   \url{"https://www.scb.se/contentassets/7a89e48960f741e08918e489ea36354a/kommunlankod_20211229.xls"}
"counties"

#' Map coordinates for Sweden municipalites
#'
#' @format A sf dataset with one row per municipality
#' \describe{
#'   \item{id}{ID of municipality with region code (as in data from SCB)}
#'   \item{geometry}{Coordinate polygon used to draw municipality borders}
#'   \item{Name}{Name of municipality}
#'   \item{RegionID}{County code}
#'   \item{MunicipID}{Municipality code}
#' }
#' @example man/examples/map_plot.R
#'
#' @source
#'   \url{http://api.thenmap.net/v2/se-7/geo/2020-06-03}
#'   \url{"https://www.scb.se/contentassets/7a89e48960f741e08918e489ea36354a/kommunlankod_20211229.xls"}
"municipalities"
