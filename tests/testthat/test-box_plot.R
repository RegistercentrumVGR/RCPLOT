test_that("box_plot works", {
  df <- data.frame(
    x = "a",
    y = 0.5,
    y_lower = 0.75,
    y_upper = 0.25,
    y_min = 0.1,
    y_max = 0.9
  )

  plt <- box_plot(
    df = df,
    x_var = "x",
    y_var = "y",
    y_lower = "y_lower",
    y_upper = "y_upper",
    y_min = "y_min",
    y_max = "y_max"
  )

  vdiffr::expect_doppelganger("box plot", plt)

  df <- df |>
    dplyr::mutate(obfuscate_reason = NA)

  box_plot(
    df = df,
    x_var = "x",
    y_var = "y",
    y_lower = "y_lower",
    y_upper = "y_upper",
    y_min = "y_min",
    y_max = "y_max"
  ) |>
    expect_no_error()
})
