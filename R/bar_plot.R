#' Bar plot function
#'
#' Plot a bar plot using ggplot2.
#'
#' @param df                 Data frame.
#' @param x_var              Variable for x axis, use string name.
#'                           Recommended that `x_var` is in character in df.
#' @param fill_var           Variable for the different colors in bars,
#'                             use string name.
#'                             Use `NULL` if only one color for bars.
#' @param y_var              Variable for y axis, if `NULL`, count is used.
#' @param style              3 different styles of bar plots,
#'                            "stack", "fill", or "dodge".
#'                            fill requires `y_percent = TRUE`.
#' @param group_by_x_var     Only relevant for style dodge. Boolean indicating
#'                             if percentages should be for `x_var` or
#'                             `fill_var`.
#' @param y_percent          If `TRUE`, y axis is in percent form.
#'                             Otherwise in count form.
#' @param percent_accuracy   Set accuracy for [scales::percent_format()].
#' @param y_lim              Limit on y axis.
#' @param x_breaks,y_breaks  Length between each break on x/y axis.
#' @param y_breaks_end       Break end, default for 100,000. Works for all count
#'                             values below that.
#' @param expand             If `TRUE`, the margins around the data are kept.
#' @param flip               If `TRUE`, x and y axis changes positions making
#'                             the bars go horizontally instead of vertically.
#' @param title              Plot title, `NULL` if no title.
#' @param subtitle           Small text under title, `NULL` if no subtitle.
#' @param y_lab              Y-axis label, use `NULL` for no label.
#' @param x_lab              X-axis label, use `NULL` for no label.
#' @param fill_colors        Color of the different categories in `fill_var`.
#' @param legend_labels      Label for each legend key.
#' @param label_breaks       Order of the legend keys.
#' @param legend_row         How many rows for the legends.
#' @param legend_col         How many columns for the legends.
#' @param bar_width          Bar width in geom_bar.
#' @param label_number       Whether or not to use scales::label_number when
#'                            y_percent is FALSE. This is useful since it
#'                            disables scientific notation when dealing with
#'                            large numbers.
#' @param ...                arguments passed to [theme_rc()].
#'
#' @return                   ggplot object containing bar plot.
#' @export
bar_plot <-
  function(df,
           x_var,
           fill_var = NULL,
           y_var = NULL,
           style = c("stack", "fill", "dodge")[1],
           group_by_x_var = TRUE,
           y_percent = TRUE,
           percent_accuracy = 1,
           y_lim = NULL,
           y_breaks = 2000,
           x_breaks = NULL,
           y_breaks_end = 100000,
           title = NULL,
           subtitle = NULL,
           y_lab = NULL,
           x_lab = NULL,
           fill_colors = NULL,
           legend_labels = ggplot2::waiver(),
           label_breaks = ggplot2::waiver(),
           legend_row = NULL,
           legend_col = NULL,
           expand = FALSE,
           flip = FALSE,
           bar_width = 0.5,
           label_number = TRUE,
           ...) {
    lifecycle::deprecate_warn(
      when = "1.1.0", what = "bar_plot()", with = "bar_plot_2()"
    )
    # Fill colors ------------------------------------------------------------
    if (is.null(fill_colors)) {
      n <- if (!is.null(fill_var)) length(unique(df[[fill_var]])) else NULL
      fill_colors <- colors_rc(n)
    }

    # If y_var != NULL, no summarise is needed. -------------------------------

    show_legend <- TRUE

    if (is.character(y_var)) {
      names(df)[names(df) == y_var] <- "y"

      if (!is.character(fill_var)) {
        df$y2 <- 1
        fill_var <- "fill_var"
        show_legend <- FALSE
      } else {
        # y2 used for style dodge ----------------------------------------------

        if (group_by_x_var) {
          df <-
            df |>
            dplyr::group_by(.data[[x_var]]) |>
            dplyr::mutate(y2 = sum(.data$y))
        } else {
          df <-
            df |>
            dplyr::group_by(.data[[fill_var]]) |>
            dplyr::mutate(y2 = sum(.data$y))
        }
      }
    } else {
      # Only one fill variabel used means no legend needed  --------------------

      if (!is.character(fill_var)) {
        fill_var <- "fill_var"
        show_legend <- FALSE
        df <-
          df |>
          dplyr::group_by(.data[[x_var]]) |>
          dplyr::summarise(y = dplyr::n())
        df$y2 <- 1
      } else {
        # Data transformations -------------------------------------------------

        if (group_by_x_var) {
          df <-
            df |>
            dplyr::group_by(.data[[x_var]], .data[[fill_var]]) |>
            dplyr::summarise(y = dplyr::n()) |>
            dplyr::group_by(.data[[x_var]]) |>
            dplyr::mutate(y2 = sum(.data$y))
        } else {
          df <-
            df |>
            dplyr::group_by(.data[[x_var]], .data[[fill_var]]) |>
            dplyr::summarise(y = dplyr::n()) |>
            dplyr::group_by(.data[[fill_var]]) |>
            dplyr::mutate(y2 = sum(.data$y))
        }
      }
    }

    # y2 used for style dodge

    # Ggplot ------------------------------------------------------------------

    bars <- ggplot2::ggplot(data = df) +
      ggplot2::scale_fill_manual(
        values = fill_colors,
        labels = legend_labels,
        breaks = label_breaks,
        guide  = ggplot2::guide_legend(nrow = legend_row, ncol = legend_col)
      ) +
      ggplot2::ggtitle(title, subtitle = subtitle) +
      ggplot2::labs(x = x_lab, y = y_lab) +
      theme_rc(
        subtitle = !is.null(subtitle),
        ...
      )

    if (y_percent) {
      y_breaks <- y_breaks / 100

      if (is.vector(y_lim)) {
        y_lim <- y_lim / 100
      }

      if (style == "dodge") {
        bars <-
          bars +
          ggplot2::geom_bar(
            width = bar_width,
            mapping = ggplot2::aes(
              x = .data[[x_var]],
              y = .data$y / .data$y2,
              fill = .data[[fill_var]]
            ),
            stat = "identity",
            show.legend = show_legend,
            position = ggplot2::position_dodge(width = 0.5)
          ) +
          ggplot2::scale_y_continuous(
            labels = scales::percent_format(
              accuracy = percent_accuracy,
              suffix = " %"
            ),
            breaks = seq(0, 1, by = y_breaks),
            limits = y_lim,
            expand = if (expand) ggplot2::waiver() else c(0, 0)
          )
      } else if (style == "fill") {
        bars <-
          bars +
          ggplot2::geom_bar(
            width = bar_width,
            mapping = ggplot2::aes(
              x = .data[[x_var]],
              y = .data$y / sum(.data$y),
              fill = .data[[fill_var]]
            ),
            stat = "identity",
            show.legend = show_legend,
            position = ggplot2::position_fill(vjust = 0.5, reverse = TRUE)
          ) +
          ggplot2::scale_y_continuous(
            labels = scales::percent_format(
              accuracy = percent_accuracy,
              suffix = " %"
            ),
            breaks = seq(0, 1, by = y_breaks),
            limits = y_lim,
            expand = if (expand) ggplot2::waiver() else c(0, 0)
          )
      } else {
        bars <-
          bars +
          ggplot2::geom_bar(
            width = bar_width,
            mapping = ggplot2::aes(
              x = .data[[x_var]],
              y = .data$y / sum(.data$y),
              fill = .data[[fill_var]]
            ),
            stat = "identity",
            show.legend = show_legend,
            position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE)
          ) +
          ggplot2::scale_y_continuous(
            labels = scales::percent_format(
              accuracy = percent_accuracy,
              suffix = " %"
            ),
            breaks = seq(0, 1, by = y_breaks),
            limits = y_lim,
            expand = if (expand) ggplot2::waiver() else c(0, 0)
          )
      }
    } else if (style == "fill") {
      bars <-
        bars +
        ggplot2::geom_bar(
          width = bar_width,
          mapping = ggplot2::aes(
            x = .data[[x_var]],
            y = .data$y,
            fill = if (utils::hasName(bars$data, fill_var)) {
              .data[[fill_var]]
            } else {
              fill_var
            }
          ),
          stat = "identity",
          show.legend = show_legend,
          position = ggplot2::position_fill(vjust = 0.5, reverse = TRUE)
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(0, y_breaks_end, by = y_breaks),
          limits = y_lim,
          expand = if (expand) ggplot2::waiver() else c(0, 0),
          labels = scales::label_number()
        )
    } else if (style == "dodge") {
      bars <-
        bars +
        ggplot2::geom_bar(
          width = bar_width,
          mapping = ggplot2::aes(
            x = .data[[x_var]],
            y = .data$y,
            fill = if (utils::hasName(bars$data, fill_var)) {
              .data[[fill_var]]
            } else {
              fill_var
            }
          ),
          stat = "identity",
          show.legend = show_legend,
          position = ggplot2::position_dodge(width = 0.5)
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(0, y_breaks_end, by = y_breaks),
          limits = y_lim,
          expand = if (expand) ggplot2::waiver() else c(0, 0),
          labels = scales::label_number()
        )
    } else {
      bars <-
        bars +
        ggplot2::geom_bar(
          width = bar_width,
          mapping = ggplot2::aes(
            x = .data[[x_var]],
            y = .data$y,
            fill = if (utils::hasName(bars$data, fill_var)) {
              .data[[fill_var]]
            } else {
              fill_var
            }
          ),
          stat = "identity",
          show.legend = show_legend,
          position = ggplot2::position_stack(vjust = 0.5, reverse = TRUE)
        ) +
        ggplot2::scale_y_continuous(
          breaks = seq(0, y_breaks_end, by = y_breaks),
          limits = y_lim,
          expand = if (expand) ggplot2::waiver() else c(0, 0),
          labels = scales::label_number()
        )
    }

    if (is.numeric(df[[x_var]]) && !is.null(x_breaks)) {
      bars <-
        bars +
        ggplot2::scale_x_continuous(
          breaks = seq(
            floor(min(df[[x_var]])),
            ceiling(max(df[[x_var]])),
            by = x_breaks
          )
        )
    }

    if (flip) {
      bars <-
        bars +
        ggplot2::coord_flip()
    }

    bars
  }

#' Produce a bar plot
#'
#' Produces a bar plot according to Registercentrum graphical standards.
#'
#' The argument `position` indicates whether to use [ggplot2::position_dodge()]
#' or [ggplot2::position_stack()] for both the bars and the labels.
#'
#' @param x_var the name of the x variable
#' @param y_var the name of the y variable
#' @param fill_var the name of the fill variable
#' @param y_var_text the name of variable used as a label
#' @param horizontal whether or not to plot horizontally
#' @param position one of `dodge` and `stack`
#' @param label whether or not to add labels to the plot
#' @param label_vjust the vertical adjustment of the labels
#' @param label_size deprecated
#' @param fill_title the title of the fill legend
#' @param add_total whether or not to add a total to the name of each bar
#' @param total_var the name of the total variable used when `add_total` is
#' `TRUE`
#' @param add_censored_caption whether or not to add a caption when data has
#' been censored, requires `obfuscated_reason` column produced by
#' [RCStat::obfuscate_data()]
#' @param arrange_by the name of the variable to arrange by
#' @param arrange_by_fill the category in fill_var that should be considered
#' in the ordering
#' @param arrange_desc whether to arrange in descending order
#' @param fill_var_order a character vector of values in `fill_var` passed to
#' [forcats::fct_relevel()] which determines the order the colored bars appear
#' in
#' @param reference_line the intercept of the horizontal reference line
#' @param legend_nrow the number of rows for the fill legend
#' @param colors manually specified colors to use for fill, only used when
#' `palette_type` is `qualitative`, must be subset of
#' `colors_rc_2(9, "qualitative")`
#' @param width the width of the bars
#' @param label_contrast whether to automatically pick the most contrasting
#' color for labels compared to the fill aesthetic
#' @param remove_grid if grid should be removed
#' @param remove_legend if all legends should be removed
#' @param labels_both_sides if both axis should have labels
#' @template plot
#' @example man/examples/bar_plot_2.R
#'
#' @export
bar_plot_2 <- function(df,
                       x_var,
                       y_var,
                       fill_var = NULL,
                       y_var_text = NULL,
                       x_breaks = ggplot2::waiver(),
                       x_labels = ggplot2::waiver(),
                       x_lim = NULL,
                       y_breaks = ggplot2::waiver(),
                       y_labels = ggplot2::waiver(),
                       y_lim = NULL,
                       horizontal = FALSE,
                       position = "dodge",
                       label = FALSE,
                       label_vjust = 0.25,
                       label_size = lifecycle::deprecated(),
                       x_lab = NULL,
                       y_lab = NULL,
                       title = ggplot2::waiver(),
                       palette_type = "qualitative",
                       fill_title = NULL,
                       add_total = FALSE,
                       total_var = NULL,
                       add_censored_caption = TRUE,
                       plotly = FALSE,
                       facet = FALSE,
                       facet_by = NULL,
                       arrange_by = x_var,
                       arrange_by_fill = NULL,
                       arrange_desc = FALSE,
                       fill_var_order = NULL,
                       reference_line = NULL,
                       legend_nrow = 1,
                       colors = NULL,
                       text_size = 7,
                       width = 0.9,
                       label_contrast = FALSE,
                       remove_grid = TRUE,
                       remove_legend = FALSE,
                       labels_both_sides = FALSE) {
  checkmate::assert_data_frame(
    df,
    min.rows = 1, min.cols = 1
  )
  checkmate::assert_choice(x_var, names(df))
  checkmate::assert_choice(y_var, names(df))
  checkmate::assert(
    checkmate::check_choice(fill_var, names(df)),
    checkmate::check_null(fill_var)
  )
  checkmate::assert(
    checkmate::check_choice(y_var_text, names(df)),
    checkmate::check_null(y_var_text)
  )
  checkmate::assert_logical(horizontal, len = 1, any.missing = FALSE)
  checkmate::assert_choice(position, c("dodge", "stack"))
  checkmate::assert_logical(label, len = 1, any.missing = FALSE)
  checkmate::assert_logical(remove_grid, len = 1, any.missing = FALSE)
  checkmate::assert_logical(remove_legend, len = 1, any.missing = FALSE)
  checkmate::assert_choice(
    palette_type, c("qualitative", "sequential", "diverging")
  )
  checkmate::assert_logical(add_total, len = 1, any.missing = FALSE)
  checkmate::assert_logical(plotly, len = 1, any.missing = FALSE)
  checkmate::assert_logical(facet, len = 1, any.missing = FALSE)
  checkmate::assert_choice(arrange_by, names(df))
  checkmate::assert(
    checkmate::check_null(fill_var_order),
    checkmate::check_character(fill_var_order)
  )
  checkmate::assert_logical(arrange_desc, len = 1, any.missing = FALSE)
  checkmate::assert(
    checkmate::check_null(reference_line),
    checkmate::check_numeric(reference_line)
  )
  checkmate::assert_numeric(
    width,
    lower = 0.01,
    upper = 1,
    any.missing = FALSE,
    len = 1
  )
  checkmate::assert_logical(
    label_contrast,
    len = 1,
    any.missing = FALSE
  )

  if (lifecycle::is_present(label_size)) {
    lifecycle::deprecate_warn(
      when = "1.2.2",
      what = "bar_plot_2(label_size)",
      with = "bar_plot_2(text_size)"
    )
  }

  caption <- NULL

  if (!(is.factor(df[[x_var]]) || is.character(df[[x_var]]))) {
    df <- df |>
      dplyr::mutate(
        !!x_var := as.character(.data[[x_var]])
      )
  }

  if (add_total) {
    checkmate::assert_choice(total_var, names(df))

    obfuscated <- NULL

    if (add_censored_caption && "obfuscated_reason" %in% names(df)) {
      obfuscated <- dplyr::if_else(
        grepl("[Nn] < \\d+", df[["obfuscated_reason"]]),
        "*",
        ""
      )
      if (any(obfuscated != "")) {
        caption <- paste0(
          "* Andelen censureras p\u00e5 grund av risk f\u00f6r r\u00f6jande",
          " eller h\u00f6g os\u00e4kerhet."
        )
      }
    }

    if (is.factor(df[[x_var]])) {
      df <- df |>
        dplyr::arrange(.data[[x_var]])

      m <- match(unique(df[[x_var]]), df[[x_var]])
      new_lvls <- paste0(
        unique(df[[x_var]]),
        ", N = ",
        df[[total_var]][m],
        obfuscated
      )

      df <- df |>
        dplyr::mutate(
          !!x_var := paste0(
            .data[[x_var]], ", N = ", .data[[total_var]], obfuscated
          ),
          !!x_var := factor(
            .data[[x_var]],
            levels = new_lvls
          )
        )
    } else {
      df <- df |>
        dplyr::mutate(
          !!x_var := paste0(
            .data[[x_var]], ", N = ", .data[[total_var]], obfuscated
          )
        )
    }
  }

  if (!is.null(arrange_by)) {
    if (arrange_desc) {
      df <- df |>
        dplyr::arrange(dplyr::desc(.data[[arrange_by]]))
    } else {
      df <- df |>
        dplyr::arrange(.data[[arrange_by]])
    }

    df <- df |>
      dplyr::mutate(
        !!x_var := forcats::fct_inorder(.data[[x_var]])
      )
  }

  if (!is.null(arrange_by) && !is.null(arrange_by_fill)) {
    checkmate::check_choice(fill_var, names(df))
    checkmate::assert_choice(
      arrange_by_fill,
      unique(df |> dplyr::pull(!!fill_var))
    )

    df_order <- df |>
      dplyr::filter(.data[[fill_var]] == arrange_by_fill) |>
      dplyr::select(.data[[x_var]], .data[[arrange_by]])

    if (arrange_desc) {
      df_order <- df_order |>
        dplyr::arrange(dplyr::desc(.data[[arrange_by]])) |>
        dplyr::mutate(ord = seq_len(dplyr::n())) |>
        dplyr::select(dplyr::all_of(c(x_var, "ord")))
    } else {
      df_order <- df_order |>
        dplyr::arrange(.data[[arrange_by]]) |>
        dplyr::mutate(ord = seq_len(dplyr::n())) |>
        dplyr::select(dplyr::all_of(c(x_var, "ord")))
    }

    df <- dplyr::left_join(
      df,
      df_order,
      by = x_var
    ) |>
      dplyr::arrange(.data[["ord"]]) |>
      dplyr::mutate(
        !!x_var := forcats::fct_inorder(.data[[x_var]])
      )
  }

  if (facet) {
    checkmate::assert_choice(facet_by, names(df))
  }

  n <- 1

  if (!is.null(fill_var)) {
    df <- dplyr::mutate(
      df,
      !!fill_var := as.factor(.data[[fill_var]]),
      !!fill_var := forcats::fct_relevel(.data[[fill_var]], fill_var_order)
    )

    n <- length(unique(df[[fill_var]]))

    if (!is.null(fill_title)) {
      df <- dplyr::rename(df, !!fill_title := fill_var)
      fill_var_name <- fill_title
    } else {
      fill_var_name <- fill_var
    }
  } else {
    df <- dplyr::mutate(df, dummy_fill_var = "1")
    fill_var_name <- "dummy_fill_var"
  }

  if (position == "dodge") {
    pos <- position_dodge(width = width)
  } else if (position == "stack") {
    pos <- ggplot2::position_stack(reverse = TRUE)
  }

  if (!is.null(colors) && palette_type == "qualitative") {
    checkmate::assert_subset(colors, colors_rc_2(n = 9, type = "qualitative"))
    fill_colors <- colors
  } else {
    fill_colors <- colors_rc_2(n = n, type = palette_type)
  }

  if (labels_both_sides) {
    second_axis <- ggplot2::dup_axis()
  } else {
    second_axis <- ggplot2::waiver()
  }

  expansion <- ifelse(label && label_vjust < 0, 0.1, 0.05)

  plt <- ggplot2::ggplot(
    data = df,
    mapping = ggplot2::aes(
      x = .data[[x_var]],
      y = .data[[y_var]],
      fill = .data[[fill_var_name]]
    )
  ) +
    ggplot2::geom_col(position = pos, width = width) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = y_labels,
      limits = y_lim,
      expand = ggplot2::expansion(mult = c(0, expansion)),
      sec.axis = second_axis
    ) +
    ggplot2::scale_x_discrete(
      breaks = x_breaks,
      labels = x_labels,
      limits = x_lim
    ) +
    theme_rc(
      text_size = text_size,
      remove_grid = remove_grid,
      remove_legend = remove_legend
    ) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      title = title,
      caption = caption
    ) +
    ggplot2::scale_fill_manual(
      values = fill_colors,
      name = fill_title,
      guide = ggplot2::guide_legend(nrow = legend_nrow)
    )

  if (label) {
    if (position == "stack") {
      label_pos <- ggplot2::position_stack(vjust = 0.5, reverse = TRUE)
    } else {
      label_pos <- ggplot2::position_dodge(width = 0.9)
    }

    text_mapping <- ggplot2::aes(
      label = .data[[y_var_text]]
    )

    if (label_contrast) {
      text_mapping <- utils::modifyList(
        text_mapping,
        ggplot2::aes(
          color = ggplot2::after_scale(
            prismatic::best_contrast(
              .data$fill,
              c("black", "white")
            )
          )
        )
      )
    }

    if (horizontal) {
      plt <- plt +
        ggplot2::geom_text(
          mapping = text_mapping,
          position = label_pos,
          hjust = label_vjust,
          size = text_size * 5 / 14
        )
    } else {
      plt <- plt +
        ggplot2::geom_text(
          mapping = text_mapping,
          position = label_pos,
          vjust = label_vjust,
          size = text_size * 5 / 14
        )
    }
  }

  if (!is.null(reference_line)) {
    plt <- plt +
      ggplot2::geom_hline(yintercept = reference_line, linetype = "dashed")
  }

  if (horizontal) {
    plt <- plt +
      ggplot2::coord_flip()
  }

  if (is.null(fill_var)) {
    plt <- plt +
      ggplot2::guides(fill = "none")
  }

  if (facet) {
    plt <- plt +
      ggplot2::facet_wrap(facets = ggplot2::vars(.data[[facet_by]]))
  }

  if (plotly) {
    tooltip <- "all"

    if (is.null(fill_var)) {
      tooltip <- c("x", "y")
    }

    plt <- plotly::ggplotly(plt, tooltip = tooltip)
  }

  return(plt)
}
