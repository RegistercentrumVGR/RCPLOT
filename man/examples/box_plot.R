df <- data.frame(
  y = c(rnorm(500, 50, 10), rnorm(500, 45, 10)),
  unit = rep(letters[1:2], each = 500)
) |>
  RCStat::get_aggregate_value(
    group_cols = "unit",
    vars = list(median = "y")
  )

box_plot(
  df = df,
  x_var = "unit",
  y_var = "y_median",
  y_lower = "y_quant_25",
  y_upper = "y_quant_75",
  y_min = "y_quant_5",
  y_max = "y_quant_95",
  arrange_by = "unit",
  arrange_desc = FALSE
)

box_plot(
  df = df,
  x_var = "unit",
  y_var = "y_median",
  y_lower = "y_quant_25",
  y_upper = "y_quant_75",
  y_min = "y_quant_5",
  y_max = "y_quant_95",
  arrange_by = "total",
  arrange_desc = FALSE,
  add_total = TRUE,
  horizontal = TRUE
)

box_plot(
  df = df,
  x_var = "unit",
  y_var = "y_median",
  y_lower = "y_quant_25",
  y_upper = "y_quant_75",
  y_min = "y_quant_5",
  y_max = "y_quant_95",
  arrange_by = "total",
  arrange_desc = TRUE,
  add_total = TRUE,
  horizontal = TRUE
)
