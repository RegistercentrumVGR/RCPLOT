#' Age-pyramid plot function
#'
#' Plot an age pyramid using ggplot2.
#'
#' @param df               Data frame.
#' @param age_var          Name of age variable.
#' @param fill_var         Name of group variable (gender or other).
#' @param man_level        Name of man level, probably "Man" or "Män".
#' @param age_breaks       Each age observation in an interval.
#' @param age_labels       Label of the interval.
#' @param percent          If `TRUE`, x-axis is in percent form.
#'                           Otherwise in count form.
#' @param x_breaks         Length between each break on x-axis.
#' @param x_breaks_end     Break end, default for 100,000. Works for all count
#'                           values below that.
#' @param title,subtitle   Plot title/subtitle, `NULL` for no title.
#' @param x_lab,y_lab      X/Y-axis labels, use `NULL` for no label.
#' @param fill_colors      Colors of the groups.
#' @param legend_position  Position of the legend in plot,
#'                           if `c(1,1)`, `c(1,0)` etc, legend inside plot.
#' @param legend_labels    Label for each legend key.
#' @param label_breaks     Order of the legend keys.
#' @param legend_row,legend_col How many rows/columns for the legends.
#' @param obfuscate Whether or not to obfuscate the data using
#'  [RCStat::obfuscate_data()]
#' @param ...             arguments passed to [theme_rc()]
#'
#' @return                ggplot object containing age pyramid plot.
#' @example               man/examples/age_pyramid.R
#' @export
age_pyramid <-
  function(df,
           age_var = "Alder",
           fill_var = "Kon",
           man_level = "Man",
           age_breaks = c(0, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf),
           age_labels = c(
             "0-39", "40-44", "45-49", "50-54", "55-59",
             "60-64", "65-69", "70-74", "75-79", "80-84",
             "85+"
           ),
           percent = TRUE,
           x_breaks = 5,
           x_breaks_end = x_breaks * 1e5,
           title = "",
           subtitle = NULL,
           y_lab = NULL,
           x_lab = NULL,
           fill_colors = NULL,
           legend_position = c(0, 0),
           legend_labels = ggplot2::waiver(),
           label_breaks = ggplot2::waiver(),
           legend_row = NULL,
           legend_col = NULL,
           obfuscate = TRUE,
           ...) {

    lifecycle::deprecate_warn(
      when = "1.1.0",
      what = "age_pyramid(fill_colors)"
    )

    # Breaks and labels for x-axis (which is currently y-axis before coordflip)

    br <- seq(0, x_breaks_end, x_breaks)
    x_breaks <- unique(c(-br, 0, br))
    x_labels <- abs(x_breaks)
    n <- nrow(df)

    if (obfuscate) {
      n <- RCStat::roundc(n, -1)
    }

    # Fill colors ------------------------------------------------------------

    ncol <- length(unique(df[[fill_var]]))
    fill_colors <- colors_rc_2(n = ncol)

    # Data transformation -----------------------------------------------------

    df <-
      df %>%
      dplyr::mutate(
        {{ age_var }} :=
          cut(.data[[age_var]], breaks = age_breaks, labels = age_labels)
      ) %>%
      dplyr::group_by(.data[[fill_var]], .data[[age_var]]) %>%
      dplyr::summarise(Population = dplyr::n()) %>%
      stats::na.omit(df) %>%
      dplyr::add_tally(.data$Population) %>%
      dplyr::mutate(
        Population = replace(
          .data$Population,
          .data[[fill_var]] == man_level,
          -.data$Population
        ),
        percent = .data$Population / n * 100
      )

    if (obfuscate) {
      df <- df |>
        dplyr::mutate(
          Population = abs(.data[["Population"]])
        ) |>
        RCStat::obfuscate_data(
          total_var = "n",
          count_var = "Population",
          prop_var = "percent"
        ) |>
        dplyr::mutate(
          Population = dplyr::if_else(
            .data[[fill_var]] == man_level,
            -.data[["Population"]],
            .data[["Population"]]
          )
        )
    }

    if (percent) {
      df <- dplyr::mutate(df, Population = .data$percent)
      x_labels <- paste0(x_labels, " %")
    }

    # ggplot ------------------------------------------------------------------

    ggplot2::ggplot(
      df,
      ggplot2::aes(.data[[age_var]], .data$Population, fill = .data[[fill_var]])
    ) +
      ggplot2::geom_bar(stat = "identity") +
      ggplot2::xlab(x_lab) +
      ggplot2::ylab(y_lab) +
      ggplot2::ggtitle(title, subtitle = subtitle) +
      ggplot2::scale_y_continuous(
        breaks = x_breaks,
        labels = x_labels,
        limits = c(-max(abs(df$Population)), max(abs(df$Population)))
      ) +
      ggplot2::scale_fill_manual(
        paste("n =", n),
        values = fill_colors,
        labels = legend_labels,
        breaks = label_breaks,
        guide  = ggplot2::guide_legend(nrow = legend_row, ncol = legend_col)
      ) +
      ggplot2::coord_flip() +
      theme_rc(
        legend_position = legend_position,
        subtitle = !is.null(subtitle),
        ...
      )
  }
