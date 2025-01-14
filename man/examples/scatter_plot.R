df <- data.frame(
  x = c(rnorm(500), rnorm(500, 2)),
  y = c(rnorm(500), rnorm(500, 2)),
  sex = rep(1:2, each = 500),
  age_cat = rep(c("<18", ">=18"), each = 2, size = 500)
)

scatter_plot(
  df = df,
  x_var = "x",
  y_var = "y"
)

scatter_plot(
  df = df,
  x_var = "x",
  y_var = "y",
  color_var = "sex",
  color_title = "Kön"
)

scatter_plot(
  df = df,
  x_var = "x",
  y_var = "y",
  color_var = "sex",
  shape_var = "age_cat"
)

scatter_plot(
  df = df,
  x_var = "x",
  y_var = "y",
  color_var = "sex",
  shape_var = "age_cat",
  x_lab = "En x-variabel",
  y_lab = "En y-variabel",
  plotly = TRUE,
  color_title = "Kön",
  shape_title = "Ålderskategori"
)

scatter_plot(
  df = df,
  x_var = "x",
  y_var = "y",
  plotly = TRUE
)

scatter_plot(
  df = df,
  x_var = "x",
  y_var = "y",
  facet = TRUE,
  facet_by = "sex"
)
