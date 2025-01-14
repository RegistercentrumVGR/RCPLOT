df <- data.frame(
  x = c(rnorm(5000, mean = 4), rnorm(5000, mean = 5)),
  sex = rep(1:2, each = 5000)
)

density_plot(
  df = df,
  var = "x"
)

density_plot(
  df = df,
  var = "x",
  color_var = "sex",
  color_title = "Kön",
  y_lab = "Density",
  x_lab = "x title"
)

density_plot(
  df = df,
  var = "x",
  color_var = "sex",
  color_title = "Kön",
  y_lab = "Density",
  x_lab = "x title",
  plotly = TRUE
)

df_2 <- data.frame(
  age = 18:30,
  n = 18:30 * 1000
)

density_plot(
  df = df_2,
  var = "age",
  counted = TRUE,
  count_var = "n",
  adjust = 2,
  x_lim = c(10, 40)
)

density_plot(
  df = df_2,
  var = "age",
  counted = TRUE,
  count_var = "n",
  adjust = 2,
  x_lim = c(10, 40),
  x_lab = "Ålder",
  title = "Hej",
  plotly = TRUE
)

df_3 <- data.frame(
  x = c(rnorm(5000, mean = 4), rnorm(5000, mean = 5)),
  sex = rep(1:2, each = 5000),
  unit = letters[1:2]
)

density_plot(
  df = df_3,
  var = "x",
  color_var = "sex",
  color_title = "Kön",
  facet = TRUE,
  facet_by = "unit"
)
