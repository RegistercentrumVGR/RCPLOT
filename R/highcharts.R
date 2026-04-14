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
#' @param y_lim limits of y-axis, if `NULL` and `proportion` is `TRUE`, will be
#' set to `c(0, 100)`
#' @param y_breaks breaks of y_axis
#' @param proportion if the values are percentages
#' @param scale_percentage if percentages should the scaled
#' @param other_vars other variables to include in the tooltip, should be a
#' named list where the name will be the key in the tooltip
#' @param x_lab labels on x axis
#' @param y_lab labels on y axis
#' @param arrange_by column to sort by
#' @param arrange_desc to arrange descending
#' @param arrange_by_fill what value in `fill_var` that sort be sorted by
#' @param fill_var_order what order `fill_var` should be displayed in, can
#' alternatively be `auto_character` or `auto_numeric` to automatically sort
#' the levels
#' @param color_x_value columns that should be different color, input as list.
#' @param bar_size width of bars
#' @param normalize_prop if _prop variable should be normalized, only valid
#' for stacked bar plot.
#' @param break_x_var_names if line breaks should be done for x_var
#' @param plot_height height of plot, value is in percentages
#' @param group_color color of fill vars
#' @param legend_title title of the legend
#' @param facet_by variable in `df` with at most 2 unique values to facet by;
#' if supplied the return value is a named list of plots, one per facet level
#' @param reversed_stacks should stacks be reversed?
#'
#' @return highcharts config, or a named list of configs when `facet_by` is set
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
                                bar_size = 15,
                                normalize_prop = TRUE,
                                break_x_var_names = FALSE,
                                plot_height = 1,
                                group_color = NULL,
                                legend_title = NULL,
                                facet_by = NULL,
                                reversed_stacks = FALSE) {

  if (!is.null(facet_by)) {
    checkmate::assert_choice(facet_by, names(df))
    facet_vals <- unique(df[[facet_by]])
    checkmate::assert_true(length(facet_vals) <= 2)
    return(purrr::map(
      facet_vals,
      ~ bar_plot_highcharts(
        df = dplyr::filter(df, .data[[facet_by]] == .x),
        x_var = x_var,
        y_var = y_var,
        horizontal = horizontal,
        fill_var = fill_var,
        position = position,
        title = facet_title(title, .x),
        y_lim = y_lim,
        y_breaks = y_breaks,
        proportion = proportion,
        scale_percentage = scale_percentage,
        other_vars = other_vars,
        x_lab = x_lab,
        y_lab = y_lab,
        arrange_by = arrange_by,
        arrange_desc = arrange_desc,
        arrange_by_fill = arrange_by_fill,
        fill_var_order = fill_var_order,
        color_x_value = color_x_value,
        bar_size = bar_size,
        normalize_prop = normalize_prop,
        break_x_var_names = break_x_var_names,
        plot_height = plot_height,
        group_color = group_color,
        legend_title = legend_title,
        reversed_stacks = reversed_stacks
      )
    ))
  }

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

  if (is.null(y_lim) && proportion) {
    y_lim <- c(0, 100)
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
    group_var_order = fill_var_order,
    plot_height = plot_height,
    group_color = group_color,
    legend_title = legend_title,
    reversed_stacks = reversed_stacks
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

  if (position == "stack") {
    if (normalize_prop && !is.null(fill_var) && proportion) {
      stacking <- "percent"
    } else {
      stacking <- "normal"
    }
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
#' @param y_lim limits of y-axis, if `NULL` and `proportion` is `TRUE`, will be
#' set to `c(0, 100)`
#' @param y_breaks breaks of y_axis
#' @param proportion if the values are proportions or percentages
#' @param scale_percentage if proportions should be re-scaled to be percentages
#' @param other_vars other variables to include in the tooltip, should be a
#' named list where the name will be the key in the tooltip
#' @param x_lab labels on x axis
#' @param y_lab labels on y axis
#' @param color_var_order what order `color_var` should be displayed in, can
#' alternatively be `auto_character` or `auto_numeric` to automatically sort
#' the levels
#' @param line_size size of lines
#' @param legend_title title of the legend
#' @param facet_by variable in `df` with at most 2 unique values to facet by;
#' if supplied the return value is a named list of plots, one per facet level
#'
#' @return highcharts config, or a named list of configs when `facet_by` is set
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
                                 line_size = 8,
                                 legend_title = NULL,
                                 facet_by = NULL) {

  if (!is.null(facet_by)) {
    checkmate::assert_choice(facet_by, names(df))
    facet_vals <- unique(df[[facet_by]])
    checkmate::assert_true(length(facet_vals) <= 2)
    return(purrr::map(
      facet_vals,
      ~ line_plot_highcharts(
        df = dplyr::filter(df, .data[[facet_by]] == .x),
        x_var = x_var,
        y_var = y_var,
        color_var = color_var,
        title = facet_title(title, .x),
        y_lim = y_lim,
        y_breaks = y_breaks,
        proportion = proportion,
        scale_percentage = scale_percentage,
        other_vars = other_vars,
        x_lab = x_lab,
        y_lab = y_lab,
        color_var_order = color_var_order,
        line_size = line_size,
        legend_title = legend_title
      )
    ))
  }

  if (is.null(y_lim) && proportion) {
    y_lim <- c(0, 100)
  }

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
    group_var_order = color_var_order,
    legend_title = legend_title
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
#' @param legend_title title of the legend
#' @param facet_by variable in `df` with at most 2 unique values to facet by;
#' if supplied the return value is a named list of plots, one per facet level
#'
#'
#' @return highcharts config, or a named list of configs when `facet_by` is set
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
                                y_lab = NULL,
                                legend_title = NULL,
                                facet_by = NULL) {

  if (!is.null(facet_by)) {
    checkmate::assert_choice(facet_by, names(df))
    facet_vals <- unique(df[[facet_by]])
    checkmate::assert_true(length(facet_vals) <= 2)
    return(purrr::map(
      facet_vals,
      ~ box_plot_highcharts(
        df = dplyr::filter(df, .data[[facet_by]] == .x),
        x_var = x_var,
        y_var = y_var,
        y_lower = y_lower,
        y_upper = y_upper,
        y_min = y_min,
        y_max = y_max,
        horizontal = horizontal,
        fill_var = fill_var,
        position = position,
        title = facet_title(title, .x),
        y_lim = y_lim,
        y_breaks = y_breaks,
        other_vars = other_vars,
        x_lab = x_lab,
        y_lab = y_lab,
        legend_title = legend_title
      )
    ))
  }

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
    y_lab = y_lab,
    legend_title = legend_title
  )

  return(out)

}

#' Highchart Area Spline plot
#'
#' creates an area spline plot in highchart, outputs config for highchart
#'
#' @param df data
#' @param x_var x-axis variable
#' @param y_var y_axis variable
#' @param color_var the name of the color variable
#' @param title title of the graph
#' @param y_lim limits of y-axis, if `NULL` and `proportion` is `TRUE`, will be
#' set to `c(0, 100)`
#' @param y_breaks breaks of y_axis
#' @param proportion if the values are proportions or percentages
#' @param scale_percentage if proportions should be re-scaled to be percentages
#' @param other_vars other variables to include in the tooltip, should be a
#' named list where the name will be the key in the tooltip
#' @param x_lab labels on x axis
#' @param y_lab labels on y axis
#' @param color_var_order what order `color_var` should be displayed in, can
#' alternatively be `auto_character` or `auto_numeric` to automatically sort
#' the levels
#' @param stacking stacking mode, one of `NULL` (no stacking), `"normal"`,
#' or `"percent"`
#' @param fill_opacity opacity of the area fill, value between 0 and 1
#' @param line_size size of the spline border
#' @param group_color color of color vars
#' @param legend_title title of the legend
#'
#' @return highcharts config
#' @export
areaspline_highcharts <- function(df,
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
                                  stacking = NULL,
                                  fill_opacity = 0.5,
                                  line_size = 8,
                                  group_color = NULL,
                                  legend_title = NULL) {

  checkmate::assert_number(fill_opacity, lower = 0, upper = 1)

  if (!is.null(stacking)) {
    checkmate::assert_choice(stacking, c("normal", "percent"))
  }

  if (is.null(y_lim) && proportion) {
    y_lim <- c(0, 100)
  }

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
    type = "areaspline",
    other_vars = other_vars,
    x_lab = x_lab,
    y_lab = y_lab,
    group_var_order = color_var_order,
    group_color = group_color,
    legend_title = legend_title
  )

  plot_options <- list(
    areaspline = list(
      fillOpacity = fill_opacity
    ),
    series = list(pointWidth = line_size)
  )

  if (!is.null(stacking)) {
    plot_options$areaspline$stacking <- stacking
  }

  out <- c(
    out,
    list(plotOptions = plot_options)
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
#' @param group_var_order order of group var, can
#' alternatively be `auto_character` or `auto_numeric` to automatically sort
#' the levels
#' @param plot_height height of plot
#' @param group_color color of group variabel
#' @param legend_title title of the legend
#' @param reversed_stacks should stacks be reversed?
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
                            group_var_order = NULL,
                            plot_height = 0.8,
                            group_color = NULL,
                            legend_title = NULL,
                            reversed_stacks = NULL) {

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

  if (!is.null(group_vars) && !all(group_vars == x_var)) {
    df <- df |>
      tidyr::complete(
        !!!rlang::syms(c(x_var, group_vars)),
        fill = purrr::set_names(as.list(rep(NA, length(vars))), vars)
      )
  }

  if (!is.null(arrange_by)) {
    checkmate::assert_choice(arrange_by, names(df))
    if (arrange_desc) {
      df <- df |>
        dplyr::arrange(dplyr::desc(.data[[arrange_by]]))
    } else {
      df <- df |>
        dplyr::arrange(!is.na(.data[[arrange_by]]), .data[[arrange_by]])
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
      inverted = TRUE,
      height = paste0(100 * plot_height, "%")
    )
  } else {
    chart <- list(
      type = type,
      inverted = FALSE,
      height = paste0(100 * plot_height, "%")
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
        group_var_order = group_var_order,
        colors = group_color
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
      list(legend = list(reversed = FALSE))
    )
  }

  if (!is.null(legend_title)) {
    checkmate::assert_string(legend_title)
    out <- purrr::modify_at(
      out,
      "legend",
      c,
      list(title = list(text = legend_title))
    )
  }

  out <- out |>
    add_y_axis(
      y_lim = y_lim,
      y_breaks = y_breaks,
      proportion = proportion,
      y_lab = y_lab,
      reversed_stacks = reversed_stacks
    ) |>
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
                       y_lab = NULL,
                       reversed_stacks = NULL) {
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

  if (!is.null(reversed_stacks)) {
    checkmate::assert_logical(reversed_stacks, len = 1)
    y_axis <- c(
      y_axis,
      list(
        reversedStacks = reversed_stacks
      )
    )
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

#' Sort a character vector
#'
#' For the numeric sort, all numbers in each string is extracted and we then
#' sort by the mean of these numbers
#'
#' @param x vector of values
#'
#' @return sorted vector
sort_character <- function(x) {
  stringr::str_sort(x, locale = "sv")
}

#' @describeIn sort_character sort a numeric vector
sort_numeric <- function(x) {
  order <- stringr::str_extract_all(x, "\\d+(\\.\\d+)?") |>
    purrr::map(as.numeric) |>
    purrr::map(mean) |>
    unlist() |>
    order()
  x[order]
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
    colors <- unlist(colors)
    checkmate::assert_subset(colors, colors_rc_3(12))
  } else {
    colors <- colors_rc_3(dplyr::n_distinct(tmp$series_var), palette_type)
  }

  if (proportion && scale_percentage) {
    tmp <- tmp |>
      dplyr::mutate(y = .data$y * 100)
  }

  if (!is.null(group_var_order)) {

    if (rlang::is_string(group_var_order)) {
      if (group_var_order %in% c("auto_numeric", "auto_character")) {
        group_var_order <- switch(
          group_var_order,
          "auto_numeric" = sort_numeric,
          "auto_character" = sort_character
        )
      }
    } else {
      group_var_order <- as.character(group_var_order)
    }

    tmp <- tmp |>
      dplyr::mutate(
        dplyr::across(
          "series_var",
          ~ forcats::fct_relevel(.x, group_var_order)
        )
      )
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

facet_title <- function(title, val) {
  if (is.null(title) || title == "") return(as.character(val))

  paste0(c(title, val), collapse = ", ")
}

#' Export a highcharts config
#'
#' Converts a highcharts config to a JSON ready to paste in the highcharts
#' sandbox
#'
#' @param cfg highcharts config
#' @param write_clip should result be saved to clipboard
#'
#' @return the resulting JSON character, invisibly if write_clip is TRUE
#' @export
export_highcharts <- function(cfg, write_clip = TRUE) {
  res <- cfg |>
    list() |>
    jsonlite::toJSON(auto_unbox = TRUE, pretty = TRUE)

  if (write_clip) {
    clipr::write_clip(res)
    cli::cli_alert_success("Saved highcharts config to clipboard!")
    return(invisible(res))
  }

  res
}
