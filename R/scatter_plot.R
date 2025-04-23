#' Produce a scatter plot
#'
#' Produce a scatter plot according to Registercentrum graphical standards
#'
#' @param x_var the name of the x variable
#' @param y_var the name of the y variable
#' @param color_var the name of the color variable
#' @param shape_var the name of the shape variable
#' @param color_title the title of the color legend
#' @param shape_title the title of the shape legend
#' @param colors manually specified colors to use for fill, only used when
#' @param remove_grid if gridlines should be removed
#' `palette_type` is `qualitative`, must be subset of
#' `colors_rc_2(9, "qualitative")`
#' @template plot
#'
#' @example man/examples/scatter_plot.R
#'
#' @export
scatter_plot <- function(df,
                         x_var,
                         y_var,
                         color_var = NULL,
                         shape_var = NULL,
                         x_breaks = ggplot2::waiver(),
                         x_labels = ggplot2::waiver(),
                         x_lim = NULL,
                         y_breaks = ggplot2::waiver(),
                         y_labels = ggplot2::waiver(),
                         y_lim = NULL,
                         x_lab = ggplot2::waiver(),
                         y_lab = ggplot2::waiver(),
                         title = ggplot2::waiver(),
                         palette_type = "qualitative",
                         color_title = NULL,
                         shape_title = NULL,
                         plotly = FALSE,
                         facet = FALSE,
                         facet_by = NULL,
                         colors = NULL,
                         remove_grid = FALSE,
                         text_size = 7) {
  checkmate::assert_data_frame(
    df,
    min.rows = 1, min.cols = 1, all.missing = FALSE
  )
  checkmate::assert_choice(x_var, names(df))
  checkmate::assert_choice(y_var, names(df))
  checkmate::assert(
    checkmate::check_choice(color_var, names(df)),
    checkmate::check_null(color_var)
  )
  checkmate::assert(
    checkmate::check_choice(shape_var, names(df)),
    checkmate::check_null(shape_var)
  )
  checkmate::assert_choice(
    palette_type, c("qualitative", "sequential", "diverging")
  )
  checkmate::assert_logical(plotly, len = 1, any.missing = FALSE)
  checkmate::assert_logical(facet, len = 1, any.missing = FALSE)
  checkmate::assert_logical(remove_grid, len = 1, any.missing = FALSE)

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
    df <- dplyr::mutate(df, dummy_color_var = "1")
    color_var_name <- "dummy_color_var"
  }

  mapping <- ggplot2::aes(
    x = .data[[x_var]],
    y = .data[[y_var]],
    color = .data[[color_var_name]]
  )

  if (!is.null(shape_var)) {
    df <- dplyr::mutate(
      df,
      !!shape_var := as.factor(.data[[shape_var]])
    )

    if (!is.null(shape_title)) {
      df <- dplyr::rename(df, !!shape_title := shape_var)
      shape_var_name <- shape_title
    } else {
      shape_var_name <- shape_var
    }

    mapping <- utils::modifyList(
      mapping,
      ggplot2::aes(
        shape = .data[[shape_var_name]]
      )
    )
  }

  if (!is.null(colors) && palette_type == "qualitative") {
    checkmate::assert_subset(colors, colors_rc_2(n = 9, type = "qualitative"))
    fill_colors <- colors
  } else {
    fill_colors <- colors_rc_2(n = n, type = palette_type)
  }

  plt <- ggplot2::ggplot(
    data = df,
    mapping = mapping
  ) +
    ggplot2::geom_point() +
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
    theme_rc(text_size = text_size, remove_grid = remove_grid) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      title = title
    ) +
    ggplot2::scale_color_manual(
      values = fill_colors,
      name = color_title
    ) +
    ggplot2::scale_shape_discrete(
      name = shape_title
    )

  if (is.null(color_var)) {
    plt <- plt +
      ggplot2::guides(color = "none")
  }

  if (facet) {
    plt <- plt +
      ggplot2::facet_wrap(facets = ggplot2::vars(.data[[facet_by]]))
  }

  if (plotly) {
    tooltip <- names(mapping)

    if (is.null(color_var)) {
      tooltip <- setdiff(tooltip, "colour")
    }

    plt <- plotly::ggplotly(plt, tooltip = tooltip)
  }

  return(plt)
}
