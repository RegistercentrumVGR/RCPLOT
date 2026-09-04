test_that("map plot works", {
  # Very simple testing to see that
  # function at least returns a plot object
  expect_equal(
    c("ggplot2::ggplot", "ggplot", "ggplot2::gg", "S7_object", "gg"),
    class(
      map_plot(
        df = counties,
        fill_var = "id",
        palette_type = "sequential"
      )
    )
  )
})
