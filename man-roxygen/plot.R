#' @param df data.frame to plot
#' @param x_lab the label for the x-axis
#' @param y_lab the label for the y-axis
#' @param title the title of the plot
#' @param y_breaks vector of breaks for y-axis
#' @param y_labels vector of labels for y-labels
#' @param y_lim limits for the y-axis
#' @param x_breaks vector of breaks for x-axis
#' @param x_labels vector of labels for x-axis
#' @param x_lim limits for the x-axis
#' @param palette_type the type of palette, one of "qualitative", "sequential",
#' and "diverging"
#' @param plotly whether or not to convert the finished plot to a plotly
#' graph
#' @param facet whether or not to facet the plots using [ggplot2::facet_wrap]
#' @param facet_by the name of the variable to facet by
#'
#' @details
#' The arguments `*_lim`, `*_breaks`, and `*_labels` are passed to
#' [ggplot2::scale_y_continuous] and [ggplot2::scale_x_discrete].
#'
#' The arguments `x_lab`, `y_lab`, and `title` are passed to [ggplot2::labs()].

