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

  df <- data.frame(
    x = letters[3:1],
    y = 1:3,
    z = 1:6,
    type = 1:2
  ) |>
    dplyr::mutate(x = forcats::fct_inorder(x))

  plt <- bar_plot_2(
    df = df,
    x_var = "x",
    y_var = "z",
    fill_var = "type",
    add_total = TRUE,
    total_var = "y"
  )

  vdiffr::expect_doppelganger("x_var factor with total with fill", plt)

  df <- data.frame(
    x = factor(letters[1:3], levels = letters[4:1]),
    y = 1:3,
    total = 3:5
  )

  plt <- bar_plot_2(
    df = df,
    x_var = "x",
    y_var = "y",
    add_total = TRUE,
    total_var = "total"
  )

  vdiffr::expect_doppelganger("x_var factor with not all levels present", plt)

  set.seed(1)

  df <- data.frame(
    x = factor(sample(letters[1:3]), levels = sample(letters)),
    y = 1:3,
    total = 3:5
  )

  plt <- bar_plot_2(
    df = df,
    x_var = "x",
    y_var = "y",
    add_total = TRUE,
    total_var = "total"
  )

  vdiffr::expect_doppelganger(
    "x_var factor with random values and level order",
    plt
  )

  df <- data.frame(
    x = factor(letters[1:3], levels = letters[4:1]),
    y = 1:6,
    total = 3:5,
    type = 1:2
  )

  plt <- bar_plot_2(
    df = df,
    x_var = "x",
    y_var = "y",
    add_total = TRUE,
    total_var = "total",
    fill_var = "type"
  )

  vdiffr::expect_doppelganger(
    "x_var factor with fill and not all levels present",
    plt
  )


  df <- data.frame(
    x = factor(letters[1:3], levels = letters[4:1]),
    y = 1:3,
    total = c(3, 10, 10)
  )

  plt <- bar_plot_2(
    df = df,
    x_var = "x",
    y_var = "y",
    add_total = TRUE,
    total_var = "total"
  )

  vdiffr::expect_doppelganger(
    "x_var factor with same total",
    plt
  )

})
