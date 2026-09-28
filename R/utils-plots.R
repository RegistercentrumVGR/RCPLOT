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

  html
}

#' Display highcharts object
#'
#' Display a highcharts object created på rcplot.
#'
#' @param plt highcharts object created by rcplot
#'
#' @export
view_highcharts <- function(plt = NULL) {

  checkmate::assert_list(plt)
  html <- htmltools::tags$div(id = "container",
                              style = "width:100%; height:100%")
  htmltools::browsable(
    htmltools::tagList(
      html,
      htmltools::tags$script(src = "https://code.highcharts.com/highcharts.js"),
      htmltools::tags$script(
        htmltools::HTML(sprintf(
          "Highcharts.chart('container', %s);",
          jsonlite::toJSON(plt, auto_unbox = TRUE)
        ))
      )
    )
  )
}

#' Display a highcharts plot from a yearly report created by rcconfig
#'
#' @param yearly_report A yearly report config created by rcconfig
#' @param title Title of graph to display
#' @param id id for graph to display
#'
#' @export
view_yearly_report_hc <- function(yearly_report = NULL,
                                  title = NULL,
                                  id = NULL) {
  checkmate::assert_list(yearly_report)
  if (is.null(title) && is.null(id)) {
    cli::cli_abort("'title' or 'id' must be specified")
  }

  hc_data <- yearly_report$sections |>
    purrr::map("children") |>
    purrr::flatten() |>
    purrr::compact() |>
    purrr::map("articles") |>
    purrr::flatten() |>
    purrr::compact() |>
    purrr::map(\(article) {

      article$visualizations |>
        purrr::keep(~ identical(.x$type, "hc")) |>
        purrr::map(\(viz) {
          list(
            title = article$title,
            id = viz$id,
            data = viz$data
          )
        })

    }) |>
    purrr::flatten()

  #Get the specific graph
  hc_data <- hc_data |>
    purrr::keep(\(x) {

      matches_title <- is.null(title) ||
        identical(x$title, title)

      matches_id <- is.null(id) ||
        identical(as.character(x$id), as.character(id))

      matches_title && matches_id

    })

  if (length(hc_data) == 1) {
    view_highcharts(hc_data[[1]]$data[[1]])
  } else if (length(hc_data) > 1) {
    cli::cli_alert_info(
      "Multiple plots with same 'title' or 'id' found!"
    )
  } else {
    cli::cli_alert_info(
      "No visualization found for the specified title or id."
    )
  }

}
