#' Line plot function
#'
#' Standard line plot using ggplot2. Y-variable not necessary.
#'
#' @param df               Data frame.
#' @param x_var            Variable for x-axis, use string name.
#'                           Recommended that x_var is
#'                           in character in df (not necessary).
#' @param color_var        Variable for the different colors in lines, use
#'                           string name. Use `NULL` if only one color for
#'                           lines.
#' @param y_var            Variable for y axis, if `NULL`, count is used.
#' @param group_by_x_var   Boolean indicating if percentages should be for
#'                           `x_var` or `color_var`.
#' @param y_percent        If `TRUE`, y-axis is in percent form. Otherwise in
#'                           count form.
#' @param percent_accuracy Set accuracy for [scales::percent_format()].
#' @param y_lim            Limit on y-axis.
#' @param y_breaks         Length between each break on y-axis.
#' @param y_breaks_end     Break end, default for 100,000. Works for all count
#'                           values below that.
#' @param expand           If `TRUE`, the margins around the data are kept.
#' @param line_size        Size of the lines.
#' @param title            Plot title, `NULL` if no title.
#' @param subtitle         Small text under title, `NULL` if no subtitle.
#' @param y_lab            Y-axis label, use `NULL` for no label.
#' @param x_lab            X-axis label, use `NULL` for no label.
#' @param fill_colors      Colors of the different categories in color_var.
#' @param legend_labels    Label for each legend key.
#' @param label_breaks     Order of the legend keys.
#' @param legend_row       How many rows for the legends.
#' @param legend_col       How many columns for the legends.
#' @param ...              Arguments passed to [theme_rc()]
#'
#' @return                 Ggplot object containing line-plot.
#' @export
line_plot <-
  function(df,
           x_var,
           # Rename to fill var for consistency?
           color_var = NULL,
           y_var = NULL,
           group_by_x_var = TRUE,
           y_percent = TRUE,
           percent_accuracy = 1,
           y_lim = NULL,
           y_breaks = 2000,
           y_breaks_end = 100000,
           line_size = 1,
           title = NULL,
           subtitle = NULL,
           y_lab = NULL,
           x_lab = NULL,
           fill_colors = NULL,
           legend_labels = ggplot2::waiver(),
           label_breaks = ggplot2::waiver(),
           legend_row = NULL,
           legend_col = NULL,
           expand = TRUE,
           ...) {
    lifecycle::deprecate_warn(
      when = "1.1.0", what = "line_plot()", with = "line_plot_2()"
    )
    # Fill colors ------------------------------------------------------------
    if (is.null(fill_colors)) {
      n <- if (!is.null(color_var)) length(unique(df[[color_var]])) else NULL
      fill_colors <- colors_rc(n)
    }

    # If y_var != NULL, no summarise is needed. -------------------------------

    show_legend <- TRUE

    if (is.character(y_var)) {
      names(df)[names(df) == y_var] <- "y"
      df$y2 <- 1

      if (!is.character(color_var)) {
        color_var <- "color_var"
        show_legend <- FALSE
      }
    } else {
      # Only one fill variabel used means no legend needed  -------------------

      if (!is.character(color_var)) {
        color_var <- "color_var"
        show_legend <- FALSE
        df <-
          df |>
          dplyr::group_by(.data[[x_var]]) |>
          dplyr::summarise(y = dplyr::n()) |>
          dplyr::mutate(y2 = sum(.data$y))
      } else {
        # Data transformations ------------------------------------------------

        if (group_by_x_var) {
          df <-
            df |>
            dplyr::group_by(.data[[x_var]], .data[[color_var]]) |>
            dplyr::summarise(y = dplyr::n()) |>
            dplyr::group_by(.data[[x_var]]) |>
            dplyr::mutate(y2 = sum(.data$y))
        } else {
          df <-
            df |>
            dplyr::group_by(.data[[x_var]], .data[[color_var]]) |>
            dplyr::summarise(y = dplyr::n()) |>
            dplyr::group_by(.data[[color_var]]) |>
            dplyr::mutate(y2 = sum(.data$y))
        }
      }
    }

    # Ggplot ------------------------------------------------------------------

    lines <-
      ggplot2::ggplot(data = df) +
      ggplot2::scale_color_manual(
        values = fill_colors,
        labels = legend_labels,
        breaks = label_breaks,
        guide = ggplot2::guide_legend(nrow = legend_row, ncol = legend_col)
      ) +
      ggplot2::ylab(y_lab) +
      ggplot2::xlab(x_lab) +
      ggplot2::ggtitle(title, subtitle = subtitle) +
      theme_rc(
        subtitle = !is.null(subtitle),
        ...
      )

    if (y_percent) {
      y_breaks <- y_breaks / 100

      if (is.vector(y_lim)) {
        y_lim <- y_lim / 100
      }

      lines <-
        lines + ggplot2::geom_line(
          mapping = ggplot2::aes(
            x = .data[[x_var]],
            y = .data$y / .data$y2,
            color = if (utils::hasName(lines$data, color_var)) {
              .data[[color_var]]
            } else {
              color_var
            },
            group = if (utils::hasName(lines$data, color_var)) {
              .data[[color_var]]
            } else {
              color_var
            }
          ),
          show.legend = show_legend,
          size = line_size
        ) +
        ggplot2::scale_y_continuous(
          labels = scales::percent_format(accuracy = percent_accuracy),
          breaks = seq(0, 1, by = y_breaks),
          limits = y_lim,
          expand = if (expand) waiver() else c(0, 0)
        )
    } else {
      lines <-
        lines + ggplot2::geom_line(
          mapping = ggplot2::aes(
            x = .data[[x_var]],
            y = .data$y,
            color = if (utils::hasName(lines$data, color_var)) {
              .data[[color_var]]
            } else {
              color_var
            },
            group = if (utils::hasName(lines$data, color_var)) {
              .data[[color_var]]
            } else {
              color_var
            }
          ),
          show.legend = show_legend,
          size = line_size
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(0, y_breaks_end, by = y_breaks),
          limits = y_lim,
          expand = if (expand) waiver() else c(0, 0)
        )
    }
    lines
  }

#' Produce a line plot
#'
#' Produces a line plot according to Registercentrum graphical standards.
#'
#' @param x_var the name of the x variable
#' @param y_var the name of the y variable
#' @param linetype_var the name of the linetype variable
#' @param color_var the name of the color variable
#' @param point whether or not to add points
#' @param linetype_title the title of the linetype legend
#' @param color_title the title of the color legend
#' @param color_nrow the number of rows for the color legend
#' @param linetype_nrow the number of rows for the linetype legend
#' @param line_size the size of lines
#' @param point_size the size of pointe in lines
#' @param point_shape_var the name of the variable used to adjust the shapes of
#' the points if `point` is `TRUE`
#' @param colors manually specified colors to use for fill, only used when
#' `palette_type` is `qualitative`, must be subset of
#' `colors_rc_2(9, "qualitative")`
#' @param remove_grid if gridlines should be removed
#' @param remove_legend if all legends should be removed
#' @template plot
#' @example man/examples/line_plot_2.R
#'
#' @export
line_plot_2 <- function(df,
                        x_var,
                        y_var,
                        linetype_var = NULL,
                        color_var = NULL,
                        y_breaks = ggplot2::waiver(),
                        y_labels = ggplot2::waiver(),
                        y_lim = NULL,
                        x_breaks = ggplot2::waiver(),
                        x_labels = ggplot2::waiver(),
                        x_lim = NULL,
                        point = TRUE,
                        x_lab = NULL,
                        y_lab = NULL,
                        title = ggplot2::waiver(),
                        palette_type = "qualitative",
                        linetype_title = NULL,
                        color_title = NULL,
                        plotly = FALSE,
                        facet = FALSE,
                        facet_by = NULL,
                        color_nrow = 1,
                        linetype_nrow = 1,
                        line_size = 1,
                        point_size = 1.5,
                        point_shape_var = NULL,
                        colors = NULL,
                        remove_grid = TRUE,
                        remove_legend = FALSE,
                        text_size = 7) {
  checkmate::assert_data_frame(
    df,
    min.rows = 1, min.cols = 1
  )
  checkmate::assert_choice(x_var, names(df))
  checkmate::assert_choice(y_var, names(df))
  checkmate::assert(
    checkmate::check_choice(linetype_var, names(df)),
    checkmate::check_null(linetype_var)
  )
  checkmate::assert(
    checkmate::check_choice(color_var, names(df)),
    checkmate::check_null(color_var)
  )
  checkmate::assert_logical(point, len = 1, any.missing = FALSE)
  checkmate::assert_logical(remove_grid, len = 1, any.missing = FALSE)
  checkmate::assert_logical(remove_legend, len = 1, any.missing = FALSE)
  checkmate::assert_choice(
    palette_type, c("qualitative", "sequential", "diverging")
  )
  checkmate::assert_logical(plotly, len = 1, any.missing = FALSE)
  checkmate::assert_logical(facet, len = 1, any.missing = FALSE)
  checkmate::assert(
    checkmate::check_choice(point_shape_var, names(df)),
    checkmate::check_null(point_shape_var)
  )

  n <- ifelse(is.null(color_var), 1, length(unique(df[[color_var]])))

  if (facet) {
    checkmate::assert_choice(facet_by, names(df))
  }

  # This is a pretty ugly solution to get plotly to display nice labels in the
  # hover. The alternative would be to add a "text" aesthetic but this would be
  # even worse since we have to take into consideration that both color and
  # linetype might be null. Furthermore, we would also have to specify a
  # "group" aesthetic since otherwise ggplot would think that there "is only one
  # observation per group". This aesthetic would have to be color_var,
  # linetype_var or interaction(color_var, linetype_var) which would also be
  # an ugly solution. This solution also means we don't have to specify "name"
  # in the scale_*_*-functions to change the legend titles.
  if (!is.null(color_var)) {
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

  if (!is.null(linetype_var)) {
    if (!is.null(linetype_title)) {
      df <- dplyr::rename(df, !!linetype_title := linetype_var)

      linetype_var_name <- linetype_title
    } else {
      linetype_var_name <- linetype_var
    }

    mapping <- utils::modifyList(
      mapping,
      ggplot2::aes(
        linetype = .data[[linetype_var_name]]
      )
    )
  }

  if (!is.null(point_shape_var)) {
    shape_title <- NULL
    # Respect the potentially renamed variables
    if (point_shape_var == color_var) {
      point_shape_var <- color_var_name
      shape_title <- color_title
    } else if (point_shape_var == linetype_var) {
      point_shape_var <- linetype_var_name
      shape_title <- linetype_title
    }

    mapping <- utils::modifyList(
      mapping,
      ggplot2::aes(
        shape = .data[[point_shape_var]]
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
    ggplot2::geom_line(linewidth = line_size) +
    ggplot2::scale_x_continuous(
      breaks = x_breaks,
      labels = x_labels,
      limits = x_lim
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = y_labels,
      limits = y_lim
    ) +
    theme_rc(
      text_size = text_size,
      remove_grid = remove_grid,
      remove_legend = remove_legend
    ) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      title = title
    ) +
    ggplot2::scale_color_manual(
      values = fill_colors,
      name = color_title,
      guide = ggplot2::guide_legend(nrow = color_nrow)
    ) +
    ggplot2::scale_linetype_discrete(
      name = linetype_title,
      guide = ggplot2::guide_legend(nrow = linetype_nrow)
    )

  if (!is.null(point_shape_var)) {
    plt <- plt + ggplot2::scale_shape_discrete(
      name = shape_title
    )
  }

  if (point) {
    plt <- plt +
      ggplot2::geom_point(size = point_size)
  }

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
