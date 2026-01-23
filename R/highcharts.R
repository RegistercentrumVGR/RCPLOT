#' Highchart Bar plot
#'
#' creates a bar plot in highchart, outputs the config file
#'
#' @param df data
#' @param x_var x-axis variable
#' @param y_var y_axis variable
#' @param fill_var the name of the fill variable
#' @param horizontal whether or not the bar plot should be horizontal,
#' @param position whether or not to make the bars stack or dodge according to
#' ggplot2 semantics
#' @param title title of the graph
#' @param y_lim limits of y-axis
#' @param y_breaks breaks of y_axis
#' @param proportion if the values are percentages
#' @param scale_percentage if percentages should the scaled
#' @param other_vars other variables to include in the tooltip, should be a
#' named list where the name will be the key in the tooltip
#' @param x_lab labels on x axis
#' @param y_lab labels on y axis
#' @param arrange_by column to sort by
#' @param arrange_desc to arrange descending
#' @param arrange_by_fill what value in fill_var that sort be sorted by
#' @param fill_var_order what order of fill_Var
#' @param color_x_value columns that should be different color, input as list.
#' @param bar_size width of bars
#' @param normalize_prop if _prop variable should be normalized, only valid
#' for stacked bar plot.
#' @param break_x_var_names if line breaks should be done for x_var
#'
#' @return highcharts config
#' @export
bar_plot_highcharts <- function(df,
                                x_var,
                                y_var,
                                horizontal = FALSE,
                                fill_var = NULL,
                                position = "dodge",
                                title = "",
                                y_lim = NULL,
                                y_breaks = NULL,
                                proportion = FALSE,
                                scale_percentage = TRUE,
                                other_vars = NULL,
                                x_lab = NULL,
                                y_lab = NULL,
                                arrange_by = NULL,
                                arrange_desc = TRUE,
                                arrange_by_fill = NULL,
                                fill_var_order = NULL,
                                color_x_value = NULL,
                                bar_size = 10,
                                normalize_prop = FALSE,
                                break_x_var_names = FALSE) {

  type <- "column"

  if (break_x_var_names) {
    df <- df |>
      dplyr::rowwise() |>
      dplyr::mutate(
        enhet = dplyr::if_else(
          stringr::str_length(.data[[x_var]]) > 20 &
            stringr::str_detect(.data[[x_var]], " "),
          {
            spaces <- stringr::str_locate_all(.data[[x_var]], " ")[[1]][, 1]
            mid <- stringr::str_length(.data[[x_var]]) / 2
            split_at <- spaces[which.min(abs(spaces - mid))]
            paste0(
              stringr::str_sub(.data[[x_var]], 1, split_at - 1),
              "<br/>",
              stringr::str_sub(.data[[x_var]], split_at + 1)
            )
          },
          .data[[x_var]]
        )
      ) |>
      dplyr::ungroup()
  }

  if (normalize_prop && !is.null(fill_var) && position == "stack") {
    df <- df |>
      dplyr::group_by(.data[[x_var]]) |>
      dplyr::mutate(prop_norm = .data[[y_var]] / sum(.data[[y_var]])) |>
      dplyr::ungroup() |>
      dplyr::mutate(
        temp = .data[[y_var]],
        !!y_var := .data[["prop_norm"]],
        !!"prop_norm" := 100 * .data[["temp"]]
      ) |>
      dplyr::select(-dplyr::all_of("temp"))

    if (!is.null(other_vars)) {
      other_vars$Andel <- "prop_norm"
    } else {
      other_vars <- list("Andel" = "prop_norm")
    }
  }

  if ("obfuscated_reason" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        dplyr::across(
          dplyr::all_of(x_var),
          ~ dplyr::if_else(
            .data$obfuscated_reason == "N < 15",
            paste0(as.character(.x), "*"),
            as.character(.x),
            missing = as.character(.x)
          )
        )
      )

    if (any(!is.na(df$obfuscated_reason))) {
      caption <- list(
        text = paste0("* Indikerar att data inte ",
                      "kan visas p.g.a. risk f\u00F6r r\u00F6jande",
                      "<br/>eller f\u00F6r lite data."),
        align = "left",
        style = list(
          fontSize = "12px"
        )
      )
    } else {
      caption <- list(
        text = paste0(" "),
        align = "left",
        style = list(
          fontSize = "12px"
        )
      )
    }
  } else {
    caption <- list(
      text = paste0(" "),
      align = "left",
      style = list(
        fontSize = "12px"
      )
    )
  }

  out <- plot_highcharts(
    df = df,
    x_var = x_var,
    vars = list(y = y_var),
    title = title,
    group_vars = fill_var,
    y_lim = y_lim,
    y_breaks = y_breaks,
    proportion = proportion,
    scale_percentage = scale_percentage,
    type = type,
    other_vars = other_vars,
    horizontal = horizontal,
    x_lab = x_lab,
    y_lab = y_lab,
    arrange_by = arrange_by,
    arrange_desc = arrange_desc,
    arrange_by_group_var = arrange_by_fill,
    group_var_order = fill_var_order
  )

  if (!(is.null(color_x_value)) && is.null(fill_var)) {
    checkmate::assert_list(color_x_value)
    cats <- out$xAxis$categories
    vals <- out$series[[1]]$data

    for (nm in names(color_x_value)) {
      idx <- match(nm, cats)

      if (!is.na(idx)) {
        checkmate::assert_choice(color_x_value[[nm]],
                                 c(colors_rc_3(15),
                                   colors_rc_3(15, type = "sequential_1"),
                                   colors_rc_3(15, type = "sequential_2")))
        vals[[idx]] <- list(
          y = vals[[idx]],
          color = color_x_value[[nm]]
        )
      }
    }
    out$series[[1]]$data <- vals
  }

  stacking <- switch(
    position,
    "dodge" = NULL,
    "stack" = "normal"
  )

  if (!is.null(stacking)) {
    out <- c(
      out,
      list(
        plotOptions = list(
          column = list(stacking = stacking),
          series = list(pointWidth = bar_size)
        )
      )
    )
  } else {
    out <- c(
      out,
      list(
        plotOptions = list(
          series = list(pointWidth = bar_size)
        )
      )
    )
  }

  if (normalize_prop) {
    out$tooltip$pointFormat <- gsub("\\{point\\.y\\}%</b><br>",
                                    "",
                                    out$tooltip$pointFormat)
  }

  out <- c(out, list(caption = caption))

  return(out)
}

#' Highchart Line plot
#'
#' creates a line plot in highchart, outputs config for highchart
#'
#' @param df data
#' @param x_var x-axis variabel
#' @param y_var y_axis variabel
#' @param color_var the name of the color variable
#' @param title title of the graph
#' @param y_lim limits of y-axis
#' @param y_breaks breaks of y_axis
#' @param proportion if the values are percentages
#' @param scale_percentage if percentages should the scaled
#' @param other_vars other variables to include in the tooltip, should be a
#' named list where the name will be the key in the tooltip
#' @param x_lab labels on x axis
#' @param y_lab labels on y axis
#' @param color_var_order order of color vars
#' @param line_size size of lines
#'
#' @return highcharts config
#' @export
line_plot_highcharts <- function(df,
                                 x_var,
                                 y_var,
                                 color_var = NULL,
                                 title = "",
                                 y_lim = NULL,
                                 y_breaks = NULL,
                                 proportion = FALSE,
                                 scale_percentage = TRUE,
                                 other_vars = NULL,
                                 x_lab = NULL,
                                 y_lab = NULL,
                                 color_var_order = NULL,
                                 line_size = 8) {

  out <- plot_highcharts(
    df = df,
    x_var = x_var,
    vars = list(y = y_var),
    title = title,
    group_vars = color_var,
    y_lim = y_lim,
    y_breaks = y_breaks,
    proportion = proportion,
    scale_percentage = scale_percentage,
    type = "line",
    other_vars = other_vars,
    x_lab = x_lab,
    y_lab = y_lab,
    group_var_order = color_var_order
  )

  out <- c(
    out,
    list(
      plotOptions = list(
        series = list(pointWidth = line_size)
      )
    )
  )

  return(out)

}
#' Highchart Box plot
#'
#' creates highchart box plot, outputs highchart config
#'
#' @param df data
#' @param x_var x-axis variable
#' @param y_var y_axis variable
#' @param y_lower the 25% value
#' @param y_upper the 75% value
#' @param y_min the minimum value
#' @param y_max the maximum value
#' @param fill_var the name of the fill variable
#' @param horizontal whether or not the bar plot should be horizontal,
#' @param position whether or not to make the bars stack or dodge according to
#' ggplot2 semantics
#' @param title title of the graph
#' @param y_lim limits of y-axis
#' @param y_breaks breaks of y_axis
#' @param other_vars other variables to include in the tooltip, should be a
#' named list where the name will be the key in the tooltip
#' @param x_lab labels on x axis
#' @param y_lab labels on y axis
#'
#'
#' @return highcharts config
#' @export
box_plot_highcharts <- function(df,
                                x_var,
                                y_var,
                                y_lower,
                                y_upper,
                                y_min,
                                y_max,
                                horizontal = FALSE,
                                fill_var = NULL,
                                position = "dodge",
                                title = "",
                                y_lim = NULL,
                                y_breaks = NULL,
                                other_vars = NULL,
                                x_lab = NULL,
                                y_lab = NULL) {

  out <- plot_highcharts(
    df = df,
    x_var = x_var,
    vars = list(
      median = y_var,
      low = y_min,
      q1 = y_lower,
      q3 = y_upper,
      high = y_max
    ),
    title = title,
    group_vars = fill_var,
    y_lim = y_lim,
    y_breaks = y_breaks,
    type = "boxplot",
    other_vars = other_vars,
    horizontal = horizontal,
    x_lab = x_lab,
    y_lab = y_lab
  )

  return(out)

}

#' Create a config for a highchart plot
#'
#' @param df the data.frame to plot
#' @param x_var the name of the x variable'
#' @param vars a named list used to rename variables to proper highcharts keys
#' @param title the title of plot
#' @param group_vars the variables indicating a series to plot
#' @param y_lim limits for the y-axis
#' @param y_breaks vector of breaks for y-axis
#' @param proportion whether the y variable is a proportion
#' @param scale_percentage whether or not to scale the y-variable by 100
#' @param type the type of graph
#' @param other_vars other variables to include in the tooltip, should be a
#' named list where the name will be the key in the tooltip
#' @param horizontal if plot should be horizontal
#' @param x_lab the title of the x axis
#' @param y_lab the title of the y axis
#' @param arrange_by column to sort by
#' @param arrange_desc if sort is descending
#' @param arrange_by_group_var value in group_var to sort by
#' @param group_var_order order of group var
#'
#' @return highcharts config
#' @export
plot_highcharts <- function(df,
                            x_var,
                            vars,
                            title,
                            group_vars,
                            y_lim,
                            y_breaks,
                            proportion = FALSE,
                            scale_percentage = FALSE,
                            type,
                            other_vars,
                            horizontal = FALSE,
                            x_lab = NULL,
                            y_lab = NULL,
                            arrange_by = NULL,
                            arrange_desc = TRUE,
                            arrange_by_group_var = NULL,
                            group_var_order = NULL) {

  if (!is.null(other_vars)) {
    checkmate::assert_list(other_vars, names = "named")
    checkmate::assert_subset(unlist(other_vars), names(df))
    checkmate::assert_character(
      unlist(other_vars),
      pattern = "[a-zA-Z0-9_\\.\u00E5-\u00F6\u00C5-\u00D6]+"
    )
  }

  checkmate::assert_logical(proportion, len = 1, any.missing = FALSE)
  checkmate::assert_logical(scale_percentage, len = 1, any.missing = FALSE)

  if (!is.null(arrange_by)) {
    checkmate::assert_choice(arrange_by, names(df))
    if (arrange_desc) {
      df <- df |>
        dplyr::arrange(dplyr::desc(.data[[arrange_by]]))
    } else {
      df <- df |>
        dplyr::arrange(.data[[arrange_by]])
    }

    if (!is.null(arrange_by_group_var)) {
      df <- df |>
        dplyr::mutate(order = dplyr::if_else(
          .data[[group_vars]] == arrange_by_group_var,
          dplyr::row_number(),
          NA
        )) |>
        dplyr::group_by(.data[[x_var]]) |>
        dplyr::mutate(order = order[!is.na(order)][1]) |>
        dplyr::ungroup() |>
        dplyr::arrange(order) |>
        dplyr::select(-dplyr::all_of("order"))
    }
  }

  if (horizontal) {
    chart <- list(
      type = type,
      inverted = TRUE
    )
  } else {
    chart <- list(
      type = type,
      inverted = FALSE
    )
  }

  x_axis <- list(
    categories = I(as.character(unique(df[[x_var]])))
  )

  if (!is.null(x_lab)) {
    x_axis <- c(
      x_axis,
      list(title = list(text = x_lab))
    )
  }

  out <- list(
    title = list(
      text = title
    ),
    chart = chart,
    xAxis = x_axis,
    series = I(
      make_series(
        df = df,
        vars = vars,
        group_vars = group_vars,
        other_vars = other_vars,
        proportion = proportion,
        scale_percentage = scale_percentage,
        group_var_order = group_var_order
      )
    )
  )

  if (is.null(group_vars)) {
    out <- c(
      out,
      list(legend = list(enabled = FALSE))
    )
  } else {
    out <- c(
      out,
      list(legend = list(reversed = TRUE))
    )
  }

  out <- out |>
    add_y_axis(y_lim, y_breaks, proportion, y_lab) |>
    add_tooltip(
      proportion = proportion,
      group_vars = group_vars,
      other_vars = other_vars,
      type = type
    )

  return(out)
}

#' @describeIn plot_highcharts add the y axis to the config
#' @param out intermediate config
add_y_axis <- function(out,
                       y_lim = NULL,
                       y_breaks = NULL,
                       proportion = FALSE,
                       y_lab = NULL) {
  y_axis <- c()

  if (!is.null(y_lim)) {
    y_axis <- c(
      y_axis,
      list(min = y_lim[1], max = y_lim[2])
    )
  }

  if (!is.null(y_breaks)) {
    y_axis <- c(
      y_axis,
      list(
        tickPositions = y_breaks
      )
    )
  }

  if (!is.null(y_lab)) {
    y_axis <- c(
      y_axis,
      list(
        title = list(text = y_lab)
      )
    )
  }

  if (proportion) {
    suffix <- "%"
  } else {
    suffix <- ""
  }

  labels <- list(
    labels = list(
      format = sprintf("{value}%s", suffix)
    )
  )

  y_axis <- c(
    y_axis,
    labels
  )

  return(
    c(
      out,
      list(yAxis = y_axis)
    )
  )
}

#' @describeIn plot_highcharts add the tooltip to the config
add_tooltip <- function(out,
                        df,
                        group_vars = NULL,
                        other_vars = NULL,
                        proportion = FALSE,
                        type) {

  if (is.null(group_vars)) {
    prefix <- ""
  } else {
    prefix <- "{series.name}"
  }

  if (proportion) {
    suffix <- "%"
  } else {
    suffix <- ""
  }

  if (type == "boxplot") {
    if (prefix != "") {
      prefix <- paste0(prefix, "<br>")
    }
    main_format <- sprintf(
      "%s
      95e kvantilen: <b>{point.high}</b><br>
      75e kvantilen: <b>{point.q3}</b><br>
      Median: <b>{point.median}</b><br>
      25e kvantilen: <b>{point.q1}</b><br>
      5e kvantilen: <b>{point.low}</b><br>",
      prefix
    )
  } else {
    if (prefix != "") {
      prefix <- paste0(prefix, ": ")
    }
    main_format <- sprintf("%s<b>{point.y}%s</b>", prefix, suffix)
  }

  point_format <- paste(
    c(
      main_format,
      sprintf("%s: <b>{point.%s}</b>", names(other_vars), other_vars)
    ),
    collapse = "<br>"
  )

  tooltip <- list(
    tooltip = list(
      pointFormat = point_format
    )
  )

  return(
    c(
      out,
      tooltip
    )
  )

}

#' @describeIn plot_highcharts converts a data.frame to a highcharts series
#' representation
#' @param palette_type passed to [colors_rc_3()]
#' @param colors an optional subset of [colors_rc_3()] with
#' `palette_type = "qualitative"` and `n = 12` used to manually set colors
make_series <- function(df,
                        vars,
                        group_vars = NULL,
                        palette_type = "qualitative",
                        colors = NULL,
                        other_vars = NULL,
                        proportion = FALSE,
                        scale_percentage = TRUE,
                        group_var_order = NULL) {

  checkmate::assert_list(vars, names = "named")
  checkmate::assert_subset(unlist(vars), names(df))

  tmp <- df |>
    dplyr::mutate(
      dplyr::across(
        dplyr::all_of(group_vars), as.character
      )
    ) |>
    dplyr::rowwise() |>
    dplyr::mutate(
      series_var = paste(
        dplyr::c_across(dplyr::all_of(group_vars)),
        collapse = ", "
      )
    ) |>
    dplyr::ungroup() |>
    dplyr::rename(unlist(vars))

  if (!is.null(colors) && palette_type == "qualitative") {
    checkmate::assert_subset(colors, colors_rc_3(12))
  } else {
    colors <- colors_rc_3(dplyr::n_distinct(tmp$series_var), palette_type)
  }

  if (proportion && scale_percentage) {
    tmp <- tmp |>
      dplyr::mutate(y = .data$y * 100)
  }

  if (!is.null(group_var_order)) {
    tmp <- tmp |>
      dplyr::mutate(dplyr::across(.data$series_var,
                                  ~ factor(.x, levels = group_var_order)))
  }

  tmp <- tmp |>
    dplyr::group_by(.data$series_var) |>
    dplyr::mutate(color = colors[dplyr::cur_group_id()]) |>
    dplyr::group_by(.data$series_var, .data$color)

  if (length(vars) == 1 && is.null(other_vars)) {
    tmp |>
      dplyr::group_map(
        ~ c(
          list(
            data = I(lapply(.x$y, function(v) if (is.na(v)) NULL else v)),
            name = .y$series_var,
            color = .y$color
          )
        )
      )
  } else {
    tmp |>
      dplyr::group_map(
        ~ c(
          list(
            data = purrr::pmap(
              .x[c(names(vars), unlist(other_vars))],
              \(...) {
                vals <- list(...)
                vals <- lapply(vals, function(v) if (is.na(v)) NULL else v)
                vals
              }
            ),
            name = .y$series_var,
            color = .y$color
          )
        )
      )
  }

}
