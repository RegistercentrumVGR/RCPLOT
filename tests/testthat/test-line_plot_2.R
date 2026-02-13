test_that("line_plot_2 works", {

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

  p <- line_plot_2(
    df = df,
    x_var = "category",
    y_var = "prop",
    color_var = "unit"
  )

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")

  expect_error({
    line_plot_2(
      df = df,
      x_var = "abc",
      y_var = "prop",
      color_var = "unit"
    )
  })

  expect_error({
    line_plot_2(
      df = df,
      x_var = "category",
      y_var = "abc",
      color_var = "unit"
    )
  })

  expect_error({
    line_plot_2(
      df = df,
      x_var = "category",
      y_var = "prop",
      color_var = "abc"
    )
  })

  df <- df |>
    dplyr::mutate(obfuscated_reason = NA)

  line_plot_2(
    df = df,
    x_var = "category",
    y_var = "prop",
    color_var = "unit"
  ) |>
    expect_no_error()

  df <- data.frame(
    year = rep(2010:2012, each = 4),
    y = 10,
    quarter = 1:4
  )

  line_plot_2(
    df = df,
    x_var = "year",
    color_var = "quarter",
    y_var = "y"
  ) |>
    vdiffr::expect_doppelganger(title = "continuous color")

  line_plot_2(
    df = df,
    x_var = "year",
    color_var = "quarter",
    y_var = "y",
    color_var_order = c("2", "3", "4", "1")
  ) |>
    vdiffr::expect_doppelganger(title = "color order")

})
