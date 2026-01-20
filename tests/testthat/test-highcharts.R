test_that("add_y_axis works", {

  add_y_axis(list(chart = list(type = "line"))) |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    as.character() |>
    expect_equal(
      paste0(
        "{",
        '\"chart\":{\"type\":\"line\"}',
        ',\"yAxis\":{\"labels\":{\"format\":\"{value}\"}}',
        "}"
      )
    )

  add_y_axis(list(), y_lim = c(0, 1)) |>
    expect_equal(
      list(
        yAxis = list(
          min = 0,
          max = 1,
          labels = list(
            format = "{value}"
          )
        )
      )
    )

  add_y_axis(list(), y_breaks = c(0, 5, 10)) |>
    expect_equal(
      list(
        yAxis = list(
          tickPositions = c(0, 5, 10),
          labels = list(
            format = "{value}"
          )
        )
      )
    )

  add_y_axis(
    list(),
    proportion = TRUE
  ) |>
    expect_equal(
      list(
        yAxis = list(
          labels = list(
            format = "{value}%"
          )
        )
      )
    )

  add_y_axis(
    list(),
    proportion = FALSE
  ) |>
    expect_equal(
      list(
        yAxis = list(
          labels = list(
            format = "{value}"
          )
        )
      )
    )

  add_y_axis(
    list(),
    y_lab = "Hello World!"
  ) |>
    expect_equal(
      list(
        yAxis = list(
          title = list(
            text = "Hello World!"
          ),
          labels = list(
            format = "{value}"
          )
        )
      )
    )

})

test_that("plot_highcharts works", {

  df <- data.frame(
    x = "a",
    y = 1
  )

  # Handles length 1 data correctly
  plot_highcharts(
    df = df,
    x_var = "x",
    vars = list(y = "y"),
    title = "",
    group_vars = NULL,
    y_lim = NULL,
    y_breaks = NULL,
    type = "column",
    other_vars = NULL
  ) |>
    jsonlite::toJSON(auto_unbox = TRUE) |>
    as.character() |>
    expect_equal(
      paste0(
        "{",
        '"title":{"text":""},',
        '"chart":{"type":"column","inverted":false},',
        '"xAxis":{"categories":["a"]},',
        '"series":[{"data":[1],"name":"","color":"#116875"}],',
        '"legend":{"enabled":false},',
        '"yAxis":{"labels":{"format":"{value}"}},',
        '"tooltip":{"pointFormat":"<b>{point.y}<\\/b>"}',
        "}"
      )
    )

})

test_that("bar_plot_highcharts works", {

  df <- data.frame(
    x = rep(letters[1:3], each = 3),
    y = 1:9,
    color = paste0(LETTERS[1:3], 4:6)
  )

  bar_plot_highcharts(
    df,
    x_var = "x",
    y_var = "y",
    fill_var = "color",
    position = "stack",
    y_lim = c(0, 50),
    proportion = FALSE
  ) |>
    expect_snapshot()

  bar_plot_highcharts(
    df,
    x_var = "x",
    y_var = "y",
    fill_var = "color",
    position = "dodge",
    y_breaks = c(0, 10, 50)
  ) |>
    expect_snapshot()

  df <- data.frame(
    x = letters[1:3],
    y = 1:3
  )

  bar_plot_highcharts(df, x_var = "x", y_var = "y") |>
    expect_snapshot()

  df <- data.frame(
    x = letters[1:5],
    y = seq(0, 1, 0.25)
  )

  df |>
    bar_plot_highcharts(
      x_var = "x",
      y_var = "y",
      proportion = TRUE,
      scale_percentage = TRUE
    ) |>
    expect_snapshot()

  df <- data.frame(
    year = rep(2010:2025, each = 2),
    y = rep(c(5, 10), each = 2),
    county = rep(c("VGR", "Stockholm"), each = 2)
  )

  df |>
    bar_plot_highcharts(
      x_var = "year",
      y_var = "y",
      fill_var = "county"
    ) |>
    expect_snapshot()


  df |>
    bar_plot_highcharts(
      x_var = "year",
      y_var = "y",
      fill_var = "county",
      horizontal = TRUE,
      proportion = TRUE,
      scale_percentage = FALSE,
      y_lim = c(0, 10)
    ) |>
    expect_snapshot()

  df <- data.frame(
    n = c(5, 10),
    total = 10,
    prop = c(0.5, 1),
    county = letters[1:2]
  )

  df |>
    bar_plot_highcharts(
      x_var = "county",
      y_var = "prop",
      other_vars = list(
        Nämnare = "total",
        Täljare = "n"
      ),
      proportion = TRUE,
      scale_percentage = TRUE
    ) |>
    expect_snapshot()

  df <- data.frame(
    n = c(5, 10),
    total = 10,
    prop = c(0.5, 1),
    county = letters[1:2],
    year = rep(2023:2024, each = 2)
  )

  df |>
    bar_plot_highcharts(
      x_var = "county",
      y_var = "prop",
      fill_var = "year",
      other_vars = list(
        Nämnare = "total",
        Täljare = "n"
      ),
      proportion = TRUE,
      scale_percentage = TRUE,
      x_lab = "Hello World!"
    ) |>
    expect_snapshot()

})

test_that("line_plot_highcharts works", {

  df <- data.frame(
    year = 2010:2025,
    y = 5
  )

  df |>
    line_plot_highcharts(
      x_var = "year",
      y_var = "y"
    ) |>
    expect_snapshot()

  df <- data.frame(
    year = rep(2010:2025, each = 2),
    y = rep(c(5, 10), each = 2),
    county = rep(c("VGR", "Stockholm"), each = 2)
  )

  df |>
    line_plot_highcharts(
      x_var = "year",
      y_var = "y",
      color_var = "county"
    ) |>
    expect_snapshot()

})

test_that("box_plot_highcharts work", {

  df <- data.frame(
    x = c("a", "b"),
    median = 3,
    low = 1,
    q1 = 2,
    q3 = 4,
    high = 5,
    total = 10
  )

  box_plot_highcharts(
    df = df,
    x_var = "x",
    y_var = "median",
    y_min = "low",
    y_lower = "q1",
    y_upper = "q3",
    y_max = "high"
  ) |>
    expect_snapshot()

  box_plot_highcharts(
    df = df,
    x_var = "x",
    y_var = "median",
    y_min = "low",
    y_lower = "q1",
    y_upper = "q3",
    y_max = "high",
    fill_var = "x",
    other_vars = list(Observationer = "total"),
    horizontal = TRUE
  ) |>
    expect_snapshot()

})
