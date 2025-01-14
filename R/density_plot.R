#' Produce a density plot
#'
#' Produces a density plot according to Registercentrum graphical standards
#'
#' If the data.frame is already counted it will be uncounted using
#' [tidyr::uncount].
#'
#' @param var the name of the variable to use when creating density
#' @param color_var the name of the color variable
#' @param counted whether or not the data.frame is already counted, typically
#' the result of [dplyr::count()]
#' @param count_var the name of the count variablet, typically "n"
#' @param alpha alpha passed to [ggplot2::geom_density()]
#' @param adjust the bandwidth used when creating the density, passed to
#' [ggplot2::geom_density()]
#' @param color_title the title of the color legend
#' @param colors manually specified colors to use for fill, only used when
#' `palette_type` is `qualitative`, must be subset of
#' `colors_rc_2(9, "qualitative")`
#' @template plot
#'
#' @example man/examples/density_plot.R
#'
#' @export
density_plot <- function(df,
                         var,
                         color_var = NULL,
                         counted = FALSE,
                         count_var = NULL,
                         alpha = 0.2,
                         adjust = 1.25,
                         y_breaks = ggplot2::waiver(),
                         y_labels = scales::label_percent(),
                         y_lim = NULL,
                         x_breaks = ggplot2::waiver(),
                         x_labels = ggplot2::waiver(),
                         x_lim = NULL,
                         x_lab = NULL,
                         y_lab = NULL,
                         title = ggplot2::waiver(),
                         palette_type = "qualitative",
                         color_title = NULL,
                         plotly = FALSE,
                         facet = FALSE,
                         facet_by = NULL,
                         colors = NULL) {

  checkmate::assert_data_frame(
    df, min.rows = 1, min.cols = 1, all.missing = FALSE
  )
  checkmate::assert_choice(var, names(df))
  checkmate::assert_logical(counted, len = 1, any.missing = FALSE)
  checkmate::assert_choice(
    palette_type, c("qualitative", "sequential", "diverging")
  )

  if (counted) {
    checkmate::assert_choice(count_var, names(df))
    df <- tidyr::uncount(
      data = df,
      weights = .data[[count_var]]
    )
  }

  checkmate::assert(
    checkmate::check_choice(color_var, names(df)),
    checkmate::check_null(color_var)
  )
  checkmate::assert_logical(plotly, len = 1, any.missing = FALSE)
  checkmate::assert_logical(facet, len = 1, any.missing = FALSE)

  if (facet) {
    checkmate::assert_choice(facet_by, names(df))
  }

  n <- 1

  if (!is.null(color_var)) {
    df <- dplyr::mutate(
      df,
      !!color_var := as.factor(.data[[color_var]])
    )
    n <- length(unique(df[[color_var]]))

    if (!is.null(color_title)) {
      df <- dplyr::rename(df, !!color_title := color_var)
      color_var_name <- color_title
    } else {
      color_var_name <- color_var
    }

  } else {
    df <- dplyr::mutate(df, dummy_fill_var = "1")
    color_var_name <- "dummy_fill_var"
  }

  if (!is.null(colors) && palette_type == "qualitative") {
    checkmate::assert_subset(colors, colors_rc_2(n = 9, type = "qualitative"))
    fill_colors <- colors
  } else {
    fill_colors <- colors_rc_2(n = n, type = palette_type)
  }

  plt <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(
      x = .data[[var]],
      fill = .data[[color_var_name]],
      color = .data[[color_var_name]]
    )
  ) +
    ggplot2::geom_density(
      alpha = alpha,
      adjust = adjust
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = y_labels,
      limits = y_lim
    ) +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels,
      limits = x_lim
    ) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      title = title
    ) +
    theme_rc() +
    ggplot2::scale_fill_manual(
      values = fill_colors,
      name = color_title,
      aesthetics = c("fill", "color")
    )

  if (is.null(color_var)) {
    plt <- plt +
      ggplot2::guides(fill = "none", color = "none")
  }

  if (facet) {
    plt <- plt +
      ggplot2::facet_wrap(facets = ggplot2::vars(.data[[facet_by]]))
  }

  if (plotly) {

    tooltip <- c("x", "colour")

    if (is.null(color_var)) {
      tooltip <- "x"
    }

    plt <- plotly::ggplotly(plt, tooltip = tooltip)
  }

  return(plt)

}
