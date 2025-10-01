#' Produce a box plot
#'
#' Produces a box plot according to Registercentrum graphical standards
#'
#' @param df The data.frame to plot
#' @param x_var The name of the x variable
#' @param y_var The name of the median variable
#' @param y_lower The name of the 25% quantile variable
#' @param y_upper The name of the 75% quantile variable
#' @param y_min The name of the 5% quantile variable
#' @param y_max The name of the 95% quantile variable
#' @param fill_var The name of the fill variable
#' @param y_breaks The length of the skips between values in the y-axis
#' @param y_labels Whether or not to add labels to the plot
#' @param y_lim The limit for the y-axis
#' @param x_lab The label for the x-axis
#' @param y_lab The label for the y-axis
#' @param title The title of the graph
#' @param horizontal Whether the graph should be flipped
#' @param arrange_by The variable to order by
#' @param arrange_desc Whether to arrange in descending order
#' @param add_total If total should be added to labels on x-axis
#' @param total_col The column that should be used for adding total
#' @param fill_title The title of the fill legend
#' @param palette_type The type of palette to use, passed to
#' [rcplot::colors_rc_2()]
#' @param colors manually specified colors to use for fill, only used when
#' `palette_type` is `qualitative`, must be subset of
#' `colors_rc_2(9, "qualitative")`
#' @param remove_grid if grid should be removed
#' @param text_size passed to [rcplot::theme_rc()]
#' @example man/examples/box_plot.R
#'
#' @export
box_plot <- function(
    df,
    x_var,
    y_var,
    fill_var = NULL,
    y_lower,
    y_upper,
    y_min,
    y_max,
    y_breaks = ggplot2::waiver(),
    y_labels = ggplot2::waiver(),
    y_lim = NULL,
    x_lab = NULL,
    y_lab = NULL,
    title = ggplot2::waiver(),
    horizontal = FALSE,
    arrange_by = x_var,
    arrange_desc = TRUE,
    add_total = FALSE,
    total_col = "total",
    fill_title = NULL,
    palette_type = "qualitative",
    colors = NULL,
    remove_grid = TRUE,
    text_size = 7) {
  # Warnings
  checkmate::assert_data_frame(
    df,
    min.rows = 1, min.cols = 1
  )
  checkmate::assert_choice(x_var, names(df))
  checkmate::assert_choice(y_var, names(df))
  checkmate::assert_choice(y_lower, names(df))
  checkmate::assert_choice(y_upper, names(df))
  checkmate::assert_choice(y_min, names(df))
  checkmate::assert_choice(y_max, names(df))
  checkmate::assert_choice(arrange_by, names(df))
  checkmate::assert_choice(
    palette_type, c("qualitative", "sequential", "diverging")
  )
  checkmate::assert_logical(remove_grid, len = 1, any.missing = FALSE)

  if (add_total) {
    checkmate::assert_choice(total_col, names(df))
  }

  # Add total to x_var
  if (add_total) {
    df <- df |> dplyr::mutate(
      !!x_var := paste0(.data[[x_var]], " (N=", .data[[total_col]], ")")
    )
  }

  # Create the ordering
  if (arrange_desc) {
    df <- dplyr::arrange(df, dplyr::desc(.data[[arrange_by]]))
  } else {
    df <- dplyr::arrange(df, .data[[arrange_by]])
  }

  df <- df |>
    dplyr::mutate(
      !!x_var := forcats::fct_inorder(.data[[x_var]])
    )

  if (!is.null(fill_var)) {
    df <- dplyr::mutate(
      df,
      !!fill_var := as.factor(.data[[fill_var]])
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
    n <- 1
  }

  if (!is.null(colors) && palette_type == "qualitative") {
    checkmate::assert_subset(colors, colors_rc_2(n = 9, type = "qualitative"))
    fill_colors <- colors
  } else {
    fill_colors <- colors_rc_2(n = n, type = palette_type)
  }

  # Create the plot
  plt <- ggplot2::ggplot(
    data = df,
    ggplot2::aes(
      x = .data[[x_var]],
      fill = .data[[fill_var_name]]
    )
  ) +
    ggplot2::geom_boxplot(
      stat = "identity",
      ggplot2::aes(
        ymin = .data[[y_min]],
        lower = .data[[y_lower]],
        middle = .data[[y_var]],
        upper = .data[[y_upper]],
        ymax = .data[[y_max]]
      ),
      staplewidth = 0.25
    ) +
    ggplot2::scale_y_continuous(
      breaks = y_breaks,
      labels = y_labels,
      limits = y_lim
    ) +
    ggplot2::labs(
      x = x_lab,
      y = y_lab,
      title = title
    ) +
    rcplot::theme_rc(
      text_size = text_size,
      remove_grid = remove_grid
    ) +
    ggplot2::scale_fill_manual(
      values = fill_colors,
      name = fill_title
    )

  # If plot should be flipped
  if (horizontal) {
    plt <- plt +
      ggplot2::coord_flip()
  }

  if (is.null(fill_var)) {
    plt <- plt +
      ggplot2::guides(fill = "none")
  }

  return(plt)
}
