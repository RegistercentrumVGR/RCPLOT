pdf(NULL)

test_that("radar_plot runs without errors for a valid data frame", {
  df <- data.frame(A = c(3, 4, 5), B = c(2, 3, 4), C = c(4, 5, 3))
  expect_silent(radar_plot(df))
})

test_that("radar_plot throws an error for non-numeric columns", {
  df <- data.frame(A = c("a", "b", "c"), B = c(1, 2, 3), C = c(2, 3, 4))
  expect_error(radar_plot(df), "Not all columns are numerical")
})

test_that("radar_plot correctly handles y_max and y_min arguments", {
  df <- data.frame(A = c(1, 2), B = c(3, 4), C = c(5, 6))
  expect_silent(radar_plot(df, y_max = 10, y_min = 1))
})

test_that("radar_plot correctly handles seg parameter", {
  df <- data.frame(A = c(1, 2), B = c(3, 4), C = c(5, 6))
  expect_silent(radar_plot(df, seg = 4))
})

test_that("radar_plot uses custom colors", {
  df <- data.frame(A = c(1, 2), B = c(3, 4), C = c(5, 6))
  colors <- c("red", "blue")
  expect_silent(radar_plot(df, colors = colors))
})

test_that("radar_plot processes legend_col correctly", {
  df <- tibble::tibble(group = c("G1", "G2"), A = c(3, 4), B = c(5, 6), C = c(7, 8))
  expect_silent(radar_plot(df, legend_col = "group"))
})

test_that("radar_plot generates expected radar chart", {
  df <- data.frame(
    A = c(3, 4, 5),
    B = c(2, 3, 4),
    C = c(4, 5, 3)
  )

  plot_fn <- function() {
    radar_plot(df)
  }

  vdiffr::expect_doppelganger("basic radar chart", plot_fn)
})

