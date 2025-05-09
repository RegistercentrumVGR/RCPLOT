#' Produce a radar plot
#'
#' Produces a radar plot using `fmsb::radarchart()`, with automatic handling
#' of axis limits and colors.
#'
#' @param df A data frame containing the values for the radar plot.
#' @param seg Number of segments in the radar chart. If `NULL`, it is
#' automatically set based on `pretty(y_min:y_max)`.
#' @param palette_type selects the type of colors used for the groups in the
#' chart can be one of "qualitative", "sequential" or "diverging"
#' @param y_lim Minimum and maximum value for the radar chart axes. If `NULL`,
#' it is automatically set to 0 - the maximum value in the df + 1
#' @param legend_col The name of the column containing group names for the
#' legend.
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
  palette_type = "qualitative",
  y_lim = NULL,
  legend_col = NULL
) {

  # If legend_col !is.null, set it to be rownames
  if (!is.null(legend_col)) df <- df |> tibble::column_to_rownames(legend_col)

  # Check that data is in proper format
  if (!all(sapply(df, is.numeric))) stop("Not all columns are numerical")

  # Set y-axis limits if not provided
  if (all(is.null(y_lim[1]) | is.na(y_lim[1]))) y_lim[1] <- c(0)
  if (
    all(is.na(y_lim[2]) | is.na(y_lim[2]))
  ) y_lim[2] <- max(df, na.rm = TRUE) + 1

  # Create min and max rows dynamically
  make_boundary_row <- function(value) {
    as.data.frame(
      matrix(
        rep(
          value,
          ncol(df)
        ),
        nrow = 1,
        dimnames = list(
          NULL,
          colnames(df)
        )
      )
    )
  }

  plot_df <- dplyr::bind_rows(
    make_boundary_row(y_lim[2]),
    make_boundary_row(y_lim[1]),
    df
  )

  if (is.null(seg)) seg <- length(pretty(y_lim[1]:y_lim[2])) - 1

  fmsb::radarchart(
    plot_df,
    axistype = 1,
    seg = seg,
    pcol = rcplot::colors_rc_2(nrow(df), type = palette_type),
    maxmin = TRUE,
    pty = 16,
    caxislabels = pretty(y_lim[1]:y_lim[2], n = seg),
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
      col = rcplot::colors_rc_2(nrow(df), type = palette_type),
      text.col = "black",
      cex = 1.15,
      pt.cex = 3
    )
  }
}
