test_that("density_plot works", {

  df <- data.frame(
    x = rnorm(mean = 5, n = 10000),
    sex = sample(1:2, size = 10000, replace = TRUE)
  )

  p <- density_plot(
    df = df,
    var = "x",
    color_var = "sex"
  )

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")

  expect_error({
    density_plot(
      df = df,
      var = "abc"
    )
  })

  df_counted <- data.frame(
    x = rpois(n = 10000, lambda = 5),
    sex = sample(1:2, size = 10000, replace = TRUE)
  ) |>
    dplyr::count(sex, x)

  p <- density_plot(
    df = df_counted,
    var = "x",
    color_var = "sex",
    counted = TRUE,
    count_var = "n"
  )

  expect_s3_class(p, "gg")
  expect_s3_class(p, "ggplot")

  expect_error({
    density_plot(
      df = df_counted,
      var = "x",
      color_var = "sex",
      counted = TRUE,
      count_var = "abc"
    )
  })

})
