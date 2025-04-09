#' Produce a radar plot
#'
#' Produces a radar plot using `fmsb::radarchart()`, with automatic handling
#' of axis limits and colors.
#'
#' @param df A data frame containing the values for the radar plot.
#' @param seg Number of segments in the radar chart. If `NULL`, it is
#' automatically set based on `pretty(y_min:y_max)`.
#' @param colors A vector of colors used for the radar chart lines. Defaults to
#' `rcplot::colors_rc_2(nrow(df))`.
#' @param y_max Maximum value for the radar chart axes. If `NULL`, it is
#' automatically set to the maximum value in `df` plus one.
#' @param y_min Minimum value for the radar chart axes. Defaults to `0`.
#' @param legend_col The name of the column containing group names for the legend.
#'
#' @return A radar chart plotted using `fmsb::radarchart()`.
#' @export
#'
#' @examples
#' df <- data.frame(A = c(3, 4, 5), B = c(2, 3, 4), C = c(4, 5, 3))
#' radar_plot(df)
radar_plot <- function(
  df,
  seg = NULL,
  colors = rcplot::colors_rc_2(nrow(df)),
  y_max = NULL,
  y_min = NULL,
  legend_col = NULL
) {

  # If legend_col !is.null, set it to be rownames
  if (!is.null(legend_col)) df <- df |> tibble::column_to_rownames(legend_col)

  # Check that data is in proper format
  if (!all(sapply(df, is.numeric))) stop("Not all columns are numerical")

  # Set y-axis limits if not provided
  if (is.null(y_max)) y_max <- max(df, na.rm = TRUE) + 1
  if (is.null(y_min)) y_min <- 0

  # Create min and max rows dynamically
  make_boundary_row <- function(value) {
    as.data.frame(matrix(rep(value, ncol(df)), nrow = 1, dimnames = list(NULL, colnames(df))))
  }

  plot_df <- dplyr::bind_rows(make_boundary_row(y_max), make_boundary_row(y_min), df)

  if (is.null(seg)) seg <- length(pretty(y_min:y_max)) - 1

  fmsb::radarchart(
    plot_df,
    axistype = 1,
    seg = seg,
    pcol = colors,
    maxmin = TRUE,
    pty = 16,
    caxislabels = pretty(y_min:y_max, n = seg),
    plwd = 3,
    plty = 1,
    cglty = 1,
    cglcol = "grey",
    axislabcol = "black",
  )
  if (!is.null(legend_col)) {
    graphics::legend(
      x = 1,
      y = 1.35,
      legend = row.names(df),
      bty = "n",
      pch = 15,
      col = colors,
      text.col = "black",
      cex = 1.15,
      pt.cex = 3
    )
  }
}
