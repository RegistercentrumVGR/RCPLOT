% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bar_plot.R
\name{bar_plot}
\alias{bar_plot}
\title{Bar plot function}
\usage{
bar_plot(
  df,
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
  y_breaks_end = 1e+05,
  title = NULL,
  subtitle = NULL,
  y_lab = NULL,
  x_lab = NULL,
  fill_colors = NULL,
  legend_labels = ggplot2::waiver(),
  label_breaks = ggplot2::waiver(),
  legend_row = NULL,
  legend_col = NULL,
  theme = getOption("theme"),
  expand = FALSE,
  flip = FALSE,
  bar_width = 0.5,
  label_number = TRUE,
  ...
)
}
\arguments{
\item{df}{Data frame.}

\item{x_var}{Variable for x axis, use string name.
Recommended that \code{x_var} is in character in df.}

\item{fill_var}{Variable for the different colors in bars,
use string name.
Use \code{NULL} if only one color for bars.}

\item{y_var}{Variable for y axis, if \code{NULL}, count is used.}

\item{style}{3 different styles of bar plots,
"stack", "fill", or "dodge".
fill requires \code{y_percent = TRUE}.}

\item{group_by_x_var}{Only relevant for style dodge. Boolean indicating
if percentages should be for \code{x_var} or \code{fill_var}.}

\item{y_percent}{If \code{TRUE}, y axis is in percent form.
Otherwise in count form.}

\item{percent_accuracy}{Set accuracy for \code{\link[scales:percent_format]{scales::percent_format()}}.}

\item{y_lim}{Limit on y axis.}

\item{x_breaks, y_breaks}{Length between each break on x/y axis.}

\item{y_breaks_end}{Break end, default for 100,000. Works for all count
values below that.}

\item{title}{Plot title, \code{NULL} if no title.}

\item{subtitle}{Small text under title, \code{NULL} if no subtitle.}

\item{y_lab}{Y-axis label, use \code{NULL} for no label.}

\item{x_lab}{X-axis label, use \code{NULL} for no label.}

\item{fill_colors}{Color of the different categories in \code{fill_var}.}

\item{legend_labels}{Label for each legend key.}

\item{label_breaks}{Order of the legend keys.}

\item{legend_row}{How many rows for the legends.}

\item{legend_col}{How many columns for the legends.}

\item{theme}{Theme to use.}

\item{expand}{If \code{TRUE}, the margins around the data are kept.}

\item{flip}{If \code{TRUE}, x and y axis changes positions making
the bars go horizontally instead of vertically.}

\item{bar_width}{Bar width in geom_bar.}

\item{label_number}{Whether or not to use scales::label_number when
y_percent is FALSE. This is useful since it
disables scientific notation when dealing with
large numbers.}

\item{...}{arguments passed to \code{\link[=theme_select]{theme_select()}}.}
}
\value{
\if{html}{\out{<div class="sourceCode">}}\preformatted{              ggplot object containing bar plot.
}\if{html}{\out{</div>}}
}
\description{
Plot a bar plot using ggplot2.
}
\examples{
# Example data
df <- ggplot2::diamonds

# Style stack
bar_plot(df, 'color', 'cut', y_breaks = 2)
bar_plot(df, 'color', 'cut', y_percent = FALSE, y_breaks = 2000)

# Style stack with y variable included
df2 <-
  dplyr::group_by(df, color, cut) \%>\%
  dplyr::summarise(y = dplyr::n(), .groups = "drop_last")
bar_plot(df2, 'color', 'cut', y_var = 'y', y_breaks = 2)

# Style fill
bar_plot(df, 'color', 'cut', y_breaks = 10, style = 'fill')

# Style dodge grouped by x_var (color in this case)
bar_plot(df, 'color', 'cut', style = 'dodge', y_breaks = 10)
bar_plot(df, 'color', 'cut', style = 'dodge', y_percent = FALSE, y_breaks = 2000)

# Style dodge grouped by fill_var (cut in this case)
bar_plot(df, 'color', 'cut', style = 'dodge', group_by_x_var = FALSE, y_breaks = 10)

# Since bar_plot() returns ggplot object, it is possible to add more features
# Here we zoom the plot using coord_cartesian():
df3 <- dplyr::filter(df, clarity \%in\% c('SI1', 'SI2', 'VS2'))
bar_plot(df3, 'clarity', style = 'dodge', y_percent = FALSE, y_breaks = 2000) +
  ggplot2::coord_cartesian(ylim = c(8000, 14000))
}
