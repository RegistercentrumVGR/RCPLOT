df <- data.frame(
  unit = letters[1:3],
  category = rep(1:3, each = 3),
  n = sample(1:10, 9, replace = TRUE)
) |>
  dplyr::group_by(unit) |>
  dplyr::mutate(
    total = sum(n),
    prop = n / total
  )

bar_plot_2(
  df = df,
  x_var = "unit",
  y_var = "prop",
  fill_var = "category",
  label = TRUE,
  position = "stack",
  y_var_text = "n",
  fill_title = "Kategori",
  y_labels = scales::label_percent(),
  x_lab = "Enhet"
)

bar_plot_2(
  df = df,
  x_var = "unit",
  y_var = "prop",
  fill_var = "category",
  label = TRUE,
  position = "stack",
  y_var_text = "n",
  fill_title = "Kategori",
  y_labels = scales::label_percent(),
  x_lab = "Enhet",
  plotly = TRUE
)

bar_plot_2(
  df = df,
  x_var = "unit",
  y_var = "total",
  position = "stack",
  horizontal = TRUE,
  x_lab = "Enhet",
  y_lab = "Antal"
)

df_2 <- data.frame(
  unit = letters[1:6],
  n = c(7, 8, 4, 9, 3, 16),
  total = c(16, 19, 20, 15, 10, 21)
) |>
  dplyr::mutate(prop = n / total) |>
  RCStat::obfuscate_data(add_reason_col = TRUE)

bar_plot_2(
  df = df_2,
  x_var = "unit",
  y_var = "prop",
  add_total = TRUE,
  total_var = "total",
  y_labels = scales::label_percent(),
  add_censored_caption = TRUE
)

bar_plot_2(
  df = df_2,
  x_var = "unit",
  y_var = "prop",
  add_total = TRUE,
  total_var = "total",
  y_labels = scales::label_percent(),
  add_censored_caption = TRUE,
  plotly = TRUE,
  x_lab = "Enhet",
  y_lab = "Andel (%)",
  title = "En graf"
)

bar_plot_2(
  df = df,
  x_var = "category",
  y_var = "prop",
  facet = TRUE,
  facet_by = "unit"
)

bar_plot_2(
  df = df_2,
  x_var = "unit",
  y_var = "prop",
  add_total = TRUE,
  total_var = "total",
  y_labels = scales::label_percent(),
  add_censored_caption = TRUE,
  x_lab = "Enhet",
  y_lab = "Andel (%)",
  title = "En graf",
  arrange_by = "total"
)

bar_plot_2(
  df = df_2,
  x_var = "unit",
  y_var = "prop",
  y_labels = scales::label_percent(),
  arrange_by = "prop",
  horizontal = TRUE,
  add_total = TRUE,
  total_var = "total"
)

bar_plot_2(
  df = df_2,
  x_var = "unit",
  y_var = "prop",
  y_labels = scales::label_percent(),
  reference_line = 0.6,
  horizontal = TRUE
)

df_3 <- data.frame(
  unit = rep(c("b", "å", "a"), each = 3),
  category = c("Ja", "Delvis", "Nej"),
  n = c(13, 9, 10, 5, 5, 7, 8, 7, 14),
  total = rep(c(32, 17, 29), each = 3)
) |>
  dplyr::mutate(
    prop = n / total
  )

bar_plot_2(
  df = df_3,
  x_var = "unit",
  y_var = "prop",
  fill_var = "category",
  horizontal = TRUE,
  position = "stack",
  y_labels = scales::label_percent(),
  arrange_by = "total",
  add_total = TRUE,
  total_var = "total",
  fill_var_order = c("Ja", "Delvis", "Nej")
)

bar_plot_2(
  df = df_3,
  x_var = "unit",
  y_var = "prop",
  fill_var = "category",
  horizontal = TRUE,
  position = "stack",
  y_labels = scales::label_percent(),
  arrange_by = "unit",
  arrange_desc = TRUE,
  add_total = TRUE,
  total_var = "total",
  fill_var_order = c("Ja", "Delvis", "Nej")
)

bar_plot_2(
  df = df_3,
  x_var = "unit",
  y_var = "prop",
  fill_var = "category",
  horizontal = TRUE,
  position = "stack",
  y_labels = scales::label_percent(),
  arrange_by = "unit",
  arrange_desc = FALSE,
  add_total = TRUE,
  total_var = "total"
)

bar_plot_2(
  df = df_3,
  x_var = "unit",
  y_var = "prop",
  fill_var = "category",
  horizontal = TRUE,
  position = "stack",
  y_labels = scales::label_percent(),
  arrange_by = "unit",
  arrange_desc = FALSE,
  arrange_by_fill = "Ja"
)

