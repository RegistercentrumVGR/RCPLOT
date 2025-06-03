test_that("plotly_to_html works", {
  df <- data.frame(
    x = letters[1:3],
    y = 3:1
  )

  plt <- bar_plot_2(
    df = df,
    x_var = "x",
    y_var = "y",
    plotly = TRUE
  )

  # Impossible to test using snapshot/snapshot_file because of minuscule changes
  # Even using transform, not possible
  expect_no_error(plotly_to_html(plt))

})
