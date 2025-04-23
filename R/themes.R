#' The default theme for Registercentrum
#'
#' Implementation of graphical profile used in package by
#' default.
#'
#' @param legend_position passed to legend.posision
#' @param legend_justification passed to legend.justification
#' @param subtitle is a subtitle used in the figure (boolean)?
#' @param title_hjust,title_margin passed to
#'   `plot.title = element_text(hjust = title_hjust,
#'   margin = margin(b = title_margin)`
#' @param axis_text_angle passed to
#'   `axis.text.x = element_text(angle = axis_text_angle)`
#' @param text_size,title_size,subtitle_size Text size for most text,
#'                                             title and subtitle
#'
#' @param remove_grid If grid should be added
#' @param plot_type One of "bar" or "line" controling the major grid lines
#' @return Modified version of [theme_classic()]
#'
#' @rdname registercentrum_themes
#' @export
#' @import ggplot2
theme_rc <- function(
    axis_text_angle = NULL,
    legend_position = "bottom",
    legend_justification = legend_position,
    text_size = 7,
    subtitle_size = 8,
    title_size = 9,
    title_hjust = 0.5,
    subtitle = FALSE,
    title_margin = if (subtitle) 1 else title_size / 2,
    remove_grid = TRUE,
    plot_type = "bar") {
  checkmate::assert_choice(plot_type, c("bar", "line"))

  thm <- theme_classic() %+replace%
    theme(

      # General ---------------------------------------------------------------
      text = element_text(colour = "black", size = text_size),
      rect = element_rect(linetype = "blank"),

      # Axis ------------------------------------------------------------------

      axis.line = element_line(linewidth = 0.2),
      axis.text = element_text(size = rel(1)),
      axis.text.x = element_text(angle = axis_text_angle),
      axis.ticks.x = element_line(linewidth = 0.2),
      axis.ticks.y = element_line(linewidth = 0.2),
      axis.title.x = element_text(
        size = rel(1),
        margin = margin(t = 10, r = 0, b = 0, l = 0)
      ),
      axis.title.y = element_text(
        size = rel(1),
        margin = margin(t = 0, r = 10, b = 0, l = 0),
        angle = 90
      ),


      # Legend ----------------------------------------------------------------

      legend.background = element_rect(fill = "transparent"),
      legend.justification = legend_justification,
      legend.key.height = unit(text_size, "pt"),
      legend.position = legend_position,
      legend.text = element_text(size = rel(1)),
      legend.title = element_text(size = text_size),


      # Panel -----------------------------------------------------------------

      panel.background = element_rect(fill = "white"),
      panel.grid.major.y = element_line(
        colour = "#ADAEAE33",
        linewidth = 0.2
      ),
      panel.grid.major.x = element_line(
        colour = "#ADAEAE33",
        linewidth = 0.2
      ),

      # Plot ------------------------------------------------------------------
      plot.margin = margin(0.2, 0.4, 0.2, 0.4, unit = "cm"),
      plot.title = element_text(
        hjust  = title_hjust,
        size   = title_size,
        margin = margin(b = title_margin)
      ),
      plot.subtitle = element_text(hjust = 0.5, size = subtitle_size),
      strip.background = element_rect(
        color = "black",
        fill = "white",
        linewidth = 1,
        linetype = "solid"
      )
    )


  if (remove_grid) {
    thm <- thm %+replace%
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank()
      )
  }

  return(thm)
}
