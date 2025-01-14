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

  p <- bar_plot_2(
    df = df,
    x_var = "category",
    y_var = "prop",
    fill_var = "unit"
  )

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")

  expect_error({
    bar_plot_2(
      df = df,
      x_var = "abc",
      y_var = "prop",
      fill_var = "unit"
    )
  })

  expect_error({
    bar_plot_2(
      df = df,
      x_var = "category",
      y_var = "abc",
      fill_var = "unit"
    )
  })

  expect_error({
    bar_plot_2(
      df = df,
      x_var = "category",
      y_var = "prop",
      bar_var = "abc"
    )
  })

})
