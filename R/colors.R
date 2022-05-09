#' Helper to get colors from theme
#'
#' @param n Number of colors needed
#' @param type Color palette to use (Paired/default/Spectral)
#' @param theme_name name of theme
#'
#' @return a vector with color codes (HEX)
#' @export
#'
#' @examples
#' colors_select(3, "default", "rc")

colors_select <- function(n, type = "default", theme_name = getOption("theme")){
  # Default theme is RC theme
  if(is.null(theme_name)){
    theme_name <- "rc"
  }

  # RColorBrewer returns min 3 colors,
  # throws warning if n < 3
  if(is.null(n)){
    n <- 3
  }

  colors <-
    switch(
      EXPR = theme_name,
      rc = rc_colors(n, type),
      slr = slr_colors(n),
      ndr = ndr_colors(n, type)
    )
  return(colors[1:n])
}

#' Wrapper for RColorBrewer to get colors
#'
#' @param n number of colors
#' @param type one of
#'       `"default"`, for standard colors.
#'       `"Spectral"`, scale.
#'       `"Paired"` for pairwise colors.
#'
#' @return vector with color codes
#' @export
#'
#' @examples
#' rc_colors(2)
rc_colors <- function(n = 3, type = "default") {

  color_names <- list(
    default = "Set3",
    Spectral = "Spectral",
    Paired = "Paired"
  )
  col <- RColorBrewer::brewer.pal(
    max(n, 3), color_names[[type]])[1:n]
  return(col)
}

#' Colors used by the SLR color pallette
#'
#' @param n number of colors
#'
#' @return vector with color codes
#' @export
#'
#' @examples
#' slr_colors(2) # two colors (not yeallow and blue)
slr_colors <- function(n = 9) {
  clrs <- c(
    yellow = "#FCC557",
    blue   = "#3E92AA",
    black  = "#000000",
    purple = "#8B599B",
    orange = "#D98736",
    green  = "#64B996",
    red    = "#C90327",
    pink   = "#DC95B2",
    grey   = "#CCCCCC"
  )

  choose <-
    if(is.null(n) || n <= 1) {
      c("blue")
    }else if (n == 2){
      c("yellow", "blue")
    }else if (n == 3){
      c("yellow", "blue", "black")
    }else if (n == 4){
      c("yellow", "green", "black", "blue")
    }else if (n <= length(clrs)) {
      seq_along(clrs)
    }else {
      stop("SLR does not have that many colors!")
    }

  unname(clrs[choose])
}

#' NDR color palette
#'
#' @param n number of colors
#' @param type one of
#'       `NULL` for default
#'       `"deep"`, for dark colors.
#'       `"gyr"`, for scale green to yellow to red.
#'       `"Paired"` for pairwise colors.
#'
#' @return vector with color codes
#' @export
#'
#' @examples
#' ndr_colors(2)
ndr_colors <- function(n = 8, type = NULL){

  palet <- c(
    "#63a883", #1 green
    "#37624c", #2 dark green
    "#9B83BA", #3 light purple
    "#794A96", #4 purple
    "#FBC500", #5 yellow
    "#7BC0DA", #6 light blue
    "#007d99", #7 blue
    "#EE7774", #8 red
    "#CC0528", #9 dark red
    "#CCA000" #10 dark yellow
  )


  if(!is.null(type)){
    if(type == "Paired"){
      palet <- palet[c(6,7,3,4,1,2)]
      return(palet[1:n])
    }
    if(type == "deep"){
      palet <- palet[c(7,5,2,4)]
      return(palet[1:n])
    }
    # Good - bad / green to red
    if(type == "gyr"){
      if(n == 6)   palet <- palet[c(2, 1, 10, 5, 8, 9)]
      if(n == 7)   palet <- palet[c(2, 1, 7, 10, 5,  8, 9)]
      return(palet)

      # palet <- palet[c(10, 2, 1, 11, 8, 9, 12)]
      # # Take colors from the "sides"
      # # if n is odd, add the middle element.
      # # if n is even - skip yellow color.
      # return(c(palet[1:floor(n/2)], palet[(n%%2==1)*ceiling(length(palet)/2)],
      #          palet[ceiling(length(palet)+1 - n/2):length(palet)]))
    }

  }
  return(palet[1:n])
}
