test_that("bar_plot_2 works", {

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

  expect_error({
    bar_plot_2(
      df = df,
      x_var = "category",
      y_var = "prop",
      fill_var = "unit",
      arrange_by_fill = "d"
    )
  })


  df <- data.frame(
    unit = letters[3:1],
    prop = 0.5,
    total = 1:3
  ) |>
    dplyr::mutate(unit = forcats::fct_inorder(.data[["unit"]]))

  plt <- bar_plot_2(
    df = df,
    x_var = "unit",
    y_var = "prop"
  )

  vdiffr::expect_doppelganger("x_var factor", plt)

  plt <- bar_plot_2(
    df = df,
    x_var = "unit",
    y_var = "prop",
    add_total = TRUE,
    total_var = "total"
  )

  vdiffr::expect_doppelganger("x_var factor with total", plt)

})
