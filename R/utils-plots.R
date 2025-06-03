#' Converts a plotly object to an HTML character
#'
#' @param plt a plotly object
#'
#' @return an HTML character
#' @export
plotly_to_html <- function(plt) {

  checkmate::assert_class(plt, "plotly")

  to_html <- utils::getFromNamespace("toHTML", "htmlwidgets")

  html <- to_html(plt, knitrOptions = list(), standalone = TRUE)
  html <- htmltools::tagList(
    htmltools::tags$head(
      htmltools::tags$title(
        class(plt)[[1]]
      )
    ),
    html
  )

  rendered <- htmltools::renderTags(html)

  body_begin <- if (!grepl("<body\\b", rendered$html[1], ignore.case = TRUE)) {
    "<body>"
  }

  body_end <- if (!is.null(body_begin)) {
    "</body>"
  }

  background <- "white"

  dependencies <- c(
    paste0('<link href="https://cdn.jsdelivr.net/gh/rstudio',
           '/htmltools@0.5.8/inst/fill/fill.css" rel="stylesheet" />'),
    paste0('<script src="https://cdn.jsdelivr.net/gh/ramnathv',
           '/htmlwidgets@1.6.2/inst/www/htmlwidgets.js"></script>'),
    paste0('<script src="https://cdn.jsdelivr.net/gh/plotly',
           '/plotly.R@4.10.3/inst/htmlwidgets/plotly.js"></script>'),
    paste0('<script src="https://cdn.jsdelivr.net/gh/plotly/plotly.R@4.10.3/',
           'inst/htmlwidgets/lib/typedarray/typedarray.min.js"></script>'),
    paste0('<script src="https://cdn.jsdelivr.net/gh/rstudio',
           '/crosstalk@1.2.1/inst/lib/jquery/jquery.min.js"></script>'),
    paste0('<link href="https://cdn.jsdelivr.net/gh/rstudio/crosstalk@1.2.1/',
           'inst/www/css/crosstalk.min.css" rel="stylesheet" />'),
    paste0('<script src="https://cdn.jsdelivr.net/gh/rstudio',
           '/crosstalk@1.2.1/inst/www/js/crosstalk.min.js"></script>'),
    paste0('<link href="https://cdn.jsdelivr.net/gh/plotly/plotly.R@4.10.3/',
           'inst/htmlwidgets/lib/plotlyjs/plotly-htmlwidgets.css"',
           ' rel="stylesheet" />'),
    paste0('<script src="https://cdn.jsdelivr.net/gh/plotly',
           "/plotly.R@4.10.3/inst/htmlwidgets/lib/plotlyjs/",
           'plotly-latest.min.js"></script>')
  )

  html <- c(
    "<!DOCTYPE html>",
    sprintf('<html lang="%s">', "en"),
    "<head>",
    "<meta charset=\"utf-8\"/>",
    sprintf(
      "<style>body{background-color:%s;}</style>",
      htmltools::htmlEscape(background)
    ),
    dependencies,
    rendered$head,
    "</head>",
    body_begin,
    rendered$html,
    body_end,
    "</html>"
  )

  return(html)
}
