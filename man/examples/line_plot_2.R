df <- data.frame(
  year = 2010:2020,
  prop = pmin(rnorm(n = 11, mean = 0.8, sd = 0.1), 1),
  n = rpois(11, 25)
)

line_plot_2(
  df = df,
  x_var = "year",
  y_var = "prop",
  y_labels = scales::label_percent(),
  x_breaks = 2010:2020,
  y_lim = c(0, 1)
)

line_plot_2(
  df = df,
  x_var = "year",
  y_var = "prop",
  y_labels = scales::label_percent(),
  x_breaks = 2010:2020,
  y_lim = c(0, 1),
  plotly = TRUE
)


df_2 <- data.frame(
  year = rep(2010:2020, each = 4),
  some_var = rep(rep(c("prop", "rapporteringsgrad"), each = 2), 11),
  county = c("a", "Riket"),
  prop = pmin(rnorm(n = 22, mean = 0.8, sd = 0.1), 1),
  n = rpois(11, 25)
)

line_plot_2(
  df = df_2,
  x_var = "year",
  y_var = "prop",
  linetype_var = "some_var",
  color_var = "county",
  y_labels = scales::label_percent(),
  x_breaks = 2010:2020,
  color_title = "Region",
  x_lab = "År",
  y_lab = "Andel (%)",
  linetype_title = "Linetype"
)

line_plot_2(
  df = df_2,
  x_var = "year",
  y_var = "prop",
  linetype_var = "some_var",
  color_var = "county",
  y_labels = scales::label_percent(),
  x_breaks = 2010:2020,
  color_title = "Region",
  plotly = TRUE,
  x_lab = "År",
  y_lab = "Andel (%)",
  linetype_title = "Linetype"
)

line_plot_2(
  df = df_2,
  x_var = "year",
  y_var = "prop",
  linetype_var = "some_var",
  facet = TRUE,
  facet_by = "county"
)
