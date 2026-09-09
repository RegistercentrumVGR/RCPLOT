test_that("make_series works", {
  df <- data.frame(
    year = 2015:2024,
    n = 1:10,
    county = "VGR"
  )

  make_series(df, list(y = "n"), x_var = "year") |>
    expect_equal(
      list(
        list(
          data = I(as.list(1:10)),
          name = "",
          color = "#116875"
        )
      )
    )

  make_series(df, list(y = "n"), "county", x_var = "year") |>
    expect_equal(
      list(
        list(
          data = I(as.list(1:10)),
          name = "VGR",
          color = "#116875"
        )
      )
    )

  df <- data.frame(
    year = 2015:2024,
    n = 1:20,
    county = rep(c("VGR", "Stockholm"), each = 10)
  )

  make_series(df, list(y = "n"), "county", x_var = "year") |>
    expect_equal(
      list(
        list(
          data = I(as.list(11:20)),
          name = "Stockholm",
          color = "#116875"
        ),
        list(
          data = I(as.list(1:10)),
          name = "VGR",
          color = "#FC5930"
        )
      )
    )

  df <- data.frame(
    value = 1:4,
    county = c("VGR", "Stockholm"),
    type = rep(c("value 1", "value 2"), each = 2)
  )

  make_series(df, list(y = "value"), c("county", "type"), x_var = "county") |>
    expect_equal(
      list(
        list(
          data = I(as.list(2)),
          name = "Stockholm, value 1",
          color = "#116875"
        ),
        list(
          data = I(as.list(4)),
          name = "Stockholm, value 2",
          color = "#FC5930"
        ),
        list(
          data = I(as.list(1)),
          name = "VGR, value 1",
          color = "#6F45BB"
        ),
        list(
          data = I(as.list(3)),
          name = "VGR, value 2",
          color = "#A48C83"
        )
      )
    )

  df <- data.frame(
    year = 2023:2024,
    y = 100,
    x = 1
  )

  make_series(
    df,
    list(y = "y"),
    "year",
    palette_type = "sequential_1",
    x_var = "x"
  ) |>
    expect_equal(
      list(
        list(
          data = I(as.list(100)),
          name = "2023",
          color = "#37863A"
        ),
        list(
          data = I(as.list(100)),
          name = "2024",
          color = "#AC641C"
        )
      )
    )

  df <- data.frame(
    year = 2023:2024,
    prop = c(0.5, 1),
    n = c(50, 100),
    total = 100
  )

  make_series(
    df,
    list(y = "prop"),
    other_vars = list(Täljare = "n", Nämnare = "total"),
    x_var = "year",
    proportion = TRUE
  ) |>
    expect_equal(
      list(
        list(
          data = list(
            list(y = 50, n = 50, total = 100),
            list(y = 100, n = 100, total = 100)
          ),
          name = "",
          color = "#116875"
        )
      )
    )

  make_series(
    df,
    list(y = "prop"),
    proportion = TRUE,
    scale_percentage = TRUE,
    x_var = "year"
  ) |>
    expect_equal(
      list(
        list(
          data = I(as.list(c(50, 100))),
          name = "",
          color = "#116875"
        )
      )
    )

  df <- tidyr::expand_grid(
    y = 1,
    year = 2020:2023,
    bmi_group = c("<18.5", "18.5-29")
  )

  make_series(
    df,
    vars = list(y = "y"),
    group_vars = "bmi_group",
    group_var_order = "auto_numeric",
    x_var = "year"
  ) |>
    expect_equal(
      list(
        list(
          data = structure(list(1, 1, 1, 1), class = "AsIs"),
          name = structure(
            1L,
            levels = c("<18.5", "18.5-29"),
            class = "factor"
          ),
          color = "#116875"
        ),
        list(
          data = structure(list(1, 1, 1, 1), class = "AsIs"),
          name = structure(
            2L,
            levels = c("<18.5", "18.5-29"),
            class = "factor"
          ),
          color = "#FC5930"
        )
      )
    )

  # Decimals are rounded
  make_series(
    data.frame(
      y = c(70.51, 70),
      x = 1:2
    ),
    vars = list(y = "y"),
    x_var = "x",
    n_decimals = 1
  ) |>
    expect_equal(
      list(
        list(
          data = I(list(70.5, 70)),
          name = "",
          color = "#116875"
        )
      )
    )

  make_series(
    data.frame(
      y = c(70.51, 70),
      x = 1:2
    ),
    vars = list(y = "y"),
    x_var = "x",
    n_decimals = 0
  ) |>
    expect_equal(
      list(
        list(
          data = I(list(71, 70)),
          name = "",
          color = "#116875"
        )
      )
    )

  make_series(
    data.frame(
      y = c(70.51, 70),
      x = 1:2
    ),
    vars = list(y = "y"),
    x_var = "x",
    n_decimals = NULL
  ) |>
    expect_equal(
      list(
        list(
          data = I(list(70.51, 70)),
          name = "",
          color = "#116875"
        )
      )
    )

  df <- data.frame(
    gender = letters[1:2],
    y = 10
  )

  make_series(
    df = df,
    vars = list(y = "y"),
    group_vars = "gender",
    x_var = "gender"
  ) |>
    expect_equal(
      list(
        list(
          data = I(
            list(
              list(
                y = 10,
                color = "#116875"
              ),
              list(
                y = 10,
                color = "#FC5930"
              )
            )
          ),
          name = ""
        )
      )
    )

})


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
          title = list(
            text = "Andel"
          ),
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

  add_y_axis(
    list(),
    horizontal_line = 50
  ) |>
    expect_equal(
      list(
        yAxis = list(
          plotLines = list(
            list(
              value = 50,
              width = 2,
              dashStyle = "Dash"
            )
          ),
          labels = list(
            format = "{value}"
          )
        )
      )
    )

  add_y_axis(
    list(),
    horizontal_line = 0.5,
    proportion = TRUE,
    scale_percentage = TRUE
  ) |>
    expect_equal(
      list(
        yAxis = list(
          title = list(
            text = "Andel"
          ),
          plotLines = list(
            list(
              value = 50,
              width = 2,
              dashStyle = "Dash"
            )
          ),
          labels = list(
            format = "{value}%"
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
    other_vars = NULL,
    legend_title = "abc"
  ) |>
    expect_equal(
      list(
        title = list(text = ""),
        chart = list(
          type = "column",
          inverted = FALSE
        ),
        xAxis = list(categories = structure("a", class = "AsIs"),
                     labels = list(
                       style = list(
                         fontSize = "14px"
                       )
                     ),
                     title = list(
                       style = list(fontSize = "16px")
                     )),
        series = structure(
          list(
            list(
              data = structure(
                list(1),
                class = "AsIs"
              ),
              name = "",
              color = "#116875"
            )
          ),
          class = "AsIs"
        ),
        legend = list(
          enabled = FALSE,
          title = list(text = "abc",
                       style = list(fontSize = "16px")),
          itemStyle = list(fontSize = "14px")
        ),
        yAxis = list(labels = list(format = "{value}",
                                   style = list(fontSize = "14px")),
                     title = list(style = list(fontSize = "16px"))),
        tooltip = list(pointFormat = "<b>{point.y}</b>")
      )
    )

  df <- data.frame(
    x = letters[1:3],
    y_1 = c(NA, 1, NA),
    y_2 = c(1, 2, NA),
    y_3 = c(NA, 3, NA)
  )

  # NA is removed
  plot_highcharts(
    df = df,
    x_var = "x",
    vars = list(y = "y_1"),
    other_vars = NULL,
    group_vars = NULL,
    type = "column",
    title = "test",
    y_lim = NULL,
    y_breaks = NULL,
    remove_value = NA
  ) |>
    purrr::pluck("series", 1, "data") |>
    expect_equal(I(list(1)))

  # Nothing is removed
  plot_highcharts(
    df = df,
    x_var = "x",
    vars = list(y = "y_1"),
    other_vars = NULL,
    group_vars = NULL,
    type = "column",
    title = "test",
    y_lim = NULL,
    y_breaks = NULL,
    remove_value = 2
  ) |>
    purrr::pluck("series", 1, "data") |>
    expect_equal(I(list(NULL, 1, NULL)))

  # Last row is removed
  plot_highcharts(
    df = df,
    x_var = "x",
    vars = list(y = "y_1", y2 = "y_2", y3 = "y_3"),
    other_vars = NULL,
    group_vars = NULL,
    type = "column",
    title = "test",
    y_lim = NULL,
    y_breaks = NULL,
    remove_value = NA
  ) |>
    purrr::pluck("series", 1, "data") |>
    expect_equal(
      list(
        list(y = NULL, y2 = 1, y3 = NULL),
        list(y = 1, y2 = 2, y3 = 3)
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
      fill_var_order = c(2023, 2024)
    ) |>
    expect_snapshot()

  df <- data.frame(
    "enhet" = paste0("Enhet ", 1:10),
    "y" = 1:10
  )

  df |>
    bar_plot_highcharts(
      x_var = "enhet",
      y_var = "y",
      arrange_by = "y"
    ) |>
    expect_snapshot()

  df |>
    bar_plot_highcharts(
      x_var = "enhet",
      y_var = "y",
      arrange_by = "y",
      arrange_desc = FALSE
    ) |>
    expect_snapshot()

  df |>
    bar_plot_highcharts(
      x_var = "enhet",
      y_var = "y",
      arrange_by = "y",
      arrange_desc = FALSE,
      horizontal = TRUE,
      color_x_value = list("Enhet 3" = "#6F45BB")
    ) |>
    expect_snapshot()

  df <- data.frame(
    x = rep(letters[1:3], each = 3),
    y = 1:9
  ) |>
    dplyr::mutate(
      obfuscated_reason = dplyr::if_else(
        x == "b" & y == 5,
        "N < 15",
        NA
      ),
      y = dplyr::if_else(x == "b" & y == 5, NA, y)
    )

  df |>
    bar_plot_highcharts(
      x_var = "x",
      y_var = "y"
    ) |>
    expect_snapshot()

  data.frame(
    x = c("a", "b", "b"),
    y = 10,
    color = c(1, 1, 2)
  ) |>
    bar_plot_highcharts(
      x_var = "x",
      y_var = "y",
      fill_var = "color"
    ) |>
    expect_snapshot()

  data.frame(
    unit = "x",
    prop = c(0.25, 0.25, 0.49),
    fill = 1:3
  ) |>
    bar_plot_highcharts(
      x_var = "unit",
      y_var = "prop",
      fill_var = "fill",
      position = "stack",
      proportion = TRUE,
      normalize_prop = TRUE
    ) |>
    expect_snapshot()

  data.frame(
    unit = "x",
    prop = c(0.25, 0.25, 0.49),
    fill = 1:3
  ) |>
    bar_plot_highcharts(
      x_var = "unit",
      y_var = "prop",
      fill_var = "fill",
      position = "stack",
      proportion = TRUE,
      normalize_prop = FALSE
    ) |>
    expect_snapshot()

  df <- data.frame(
    x = forcats::fct(
      rep(letters[c(2, 3, 1)], each = 2)
    ),
    obfuscated_reason = rep(c(NA, "N < 15", NA), each = 2),
    fill = 1:2,
    y = rep(1:3, each = 2)
  )

  df |>
    bar_plot_highcharts(
      x_var = "x",
      y_var = "y",
      fill_var = "fill"
    ) |>
    purrr::pluck("xAxis", "categories") |>
    expect_equal(I(c("b", "c*", "a")))

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

  df |>
    line_plot_highcharts(
      x_var = "year",
      y_var = "y",
      x_breaks = c(2010, 2018, 2025)
    ) |>
    expect_snapshot()

  withr::local_seed(1)

  data.frame(
    y = sample(1:3, 10, TRUE),
    color = 1,
    year = 2010:2019
  ) |>
    dplyr::bind_rows(
      data.frame(
        y = sample(1:3, 8, TRUE),
        color = 2,
        year = 2011:2018
      )
    ) |>
    line_plot_highcharts(
      x_var = "year",
      y_var = "y",
      color_var = "color"
    ) |>
    expect_snapshot()

  data.frame(
    year = 2020,
    county = 1:2,
    group = "a",
    prop = 0.5
  ) |>
    dplyr::bind_rows(
      data.frame(
        year = 2020,
        county = 1:2,
        group = "b",
        prop = 0.75
      )
    ) |>
    line_plot_highcharts(
      x_var = "year",
      y_var = "prop",
      color_var = c("county", "group"),
      proportion = TRUE
    ) |>
    expect_snapshot()

  df <- data.frame(
    year = 2010:2020,
    y = c(0, 1.1034, 2:10)
  )

  expect_warning(
    res <- line_plot_highcharts(
      df = df,
      x_var = "year",
      y_var = "y",
      surv = TRUE,
      proportion = FALSE
    )
  )

  expect_equal(
    res$plotOptions$line$step,
    "right"
  )

  expect_equal(
    unlist(res$series[[1]]$data),
    c(0, 1.103, 2:10)
  )

  expect_no_warning(
    res <- line_plot_highcharts(
      df = df,
      x_var = "year",
      y_var = "y",
      surv = TRUE,
      proportion = TRUE
    )
  )

  expect_equal(
    unlist(res$series[[1]]$data),
    c(0, 110.3, seq(200, 1000, by = 100))
  )

})

test_that("line_plot_highcharts with surv = TRUE thins redundant step points", {
  df <- data.frame(
    time = 1:10,
    surv = c(1, 1, 0.9, 0.9, 0.9, 0.8, 0.8, 0.7, 0.7, 0.7)
  )

  res <- line_plot_highcharts(
    df = df,
    x_var = "time",
    y_var = "surv",
    surv = TRUE,
    proportion = TRUE,
    n_decimals = 0
  )

  # only rows where the (rounded) value changes, plus the last row, survive
  expect_equal(
    unlist(res$series[[1]]$data),
    c(100, 90, 80, 70, 70)
  )

  # thinning can leave a group with a gap at another group's x-value, so
  # nulls must be connected across for surv curves
  expect_true(res$plotOptions$line$connectNulls)

  # a group where the value never changes still keeps its first and last row
  df_flat <- data.frame(
    time = 1:5,
    surv = 1,
    grp = "a"
  )

  res_flat <- line_plot_highcharts(
    df = df_flat,
    x_var = "time",
    y_var = "surv",
    color_var = "grp",
    surv = TRUE,
    proportion = TRUE,
    n_decimals = 0
  )

  expect_equal(unlist(res_flat$series[[1]]$data), c(100, 100))

  # rounding-induced ties are also thinned away (rounding is done on the
  # same scale make_series() ultimately rounds and displays, so a
  # `proportion = TRUE` percentage tie only merges rows when the displayed
  # percentages actually match, not just the raw 0-1 values)
  df_round <- data.frame(
    time = 1:4,
    surv = c(0.901, 0.899, 0.850, 0.849)
  )

  res_round <- line_plot_highcharts(
    df = df_round,
    x_var = "time",
    y_var = "surv",
    surv = TRUE,
    proportion = FALSE,
    n_decimals = 1
  )

  expect_equal(unlist(res_round$series[[1]]$data), c(0.9, 0.8, 0.8))

  # the same raw values are NOT tied once scaled to percentages, since they
  # differ well beyond 1 decimal place once multiplied by 100
  res_round_pct <- line_plot_highcharts(
    df = df_round,
    x_var = "time",
    y_var = "surv",
    surv = TRUE,
    proportion = TRUE,
    n_decimals = 1
  )

  expect_equal(
    unlist(res_round_pct$series[[1]]$data),
    c(90.1, 89.9, 85, 84.9)
  )

  # connectNulls is only meaningful/safe for surv curves, not line charts in
  # general (see thin_step_curve() docs), so it must stay unset otherwise
  res_non_surv <- line_plot_highcharts(df, x_var = "time", y_var = "surv")
  expect_null(res_non_surv$plotOptions$line$connectNulls)

})

test_that("line_plot_highcharts marker_enabled works", {
  df <- data.frame(
    year = 2010:2020,
    y = 1:11
  )

  res_default <- line_plot_highcharts(df, x_var = "year", y_var = "y")
  expect_equal(res_default$plotOptions$line$marker, list(enabled = TRUE))

  res_disabled <- line_plot_highcharts(
    df,
    x_var = "year", y_var = "y",
    marker_enabled = FALSE
  )
  expect_equal(res_disabled$plotOptions$line$marker, list(enabled = FALSE))

  res_hover <- line_plot_highcharts(
    df,
    x_var = "year", y_var = "y",
    marker_enabled = "enable_on_hover",
    marker_size = 6
  )
  expect_equal(
    res_hover$plotOptions$line$marker,
    list(
      enabled = FALSE, states = list(hover = list(enabled = TRUE, radius = 6))
    )
  )

  expect_error(
    line_plot_highcharts(
      df,
      x_var = "year",
      y_var = "y",
      marker_enabled = "invalid"
    )
  )
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

test_that("sorting works", {
  c("18.5-29.1", "39.1", "<1-2") |>
    sort_numeric() |>
    expect_equal(
      c("<1-2", "18.5-29.1", "39.1")
    )

  letters[1:3] |>
    sort_numeric() |>
    expect_equal(letters[1:3])

  letters[3:1] |>
    sort_character() |>
    expect_equal(letters[1:3])

  c("a", "b", "å") |>
    sort_character() |>
    expect_equal(
      c("a", "b", "å")
    )
})

test_that("facet_by works", {
  withr::local_seed(1)

  df <- data.frame(
    type = 1:2,
    x = 2010:2020,
    prop = sample(1:10, 22, TRUE) / 10
  )

  res <- line_plot_highcharts(
    df,
    x_var = "x",
    y_var = "prop",
    facet_by = "type",
    proportion = TRUE
  )

  expect_snapshot(res)
})

test_that("areaspline_highcharts works", {
  # Basic case, no grouping
  df <- data.frame(
    year = 2010:2025,
    y = 5
  )

  df |>
    areaspline_highcharts(
      x_var = "year",
      y_var = "y"
    ) |>
    expect_snapshot()

  # With color_var
  df <- data.frame(
    year = rep(2010:2025, each = 2),
    y = rep(c(5, 10), each = 2),
    county = rep(c("VGR", "Stockholm"), each = 2)
  )

  df |>
    areaspline_highcharts(
      x_var = "year",
      y_var = "y",
      color_var = "county"
    ) |>
    expect_snapshot()

  # Proportion with y_lim auto-set
  df <- data.frame(
    year = 2020:2024,
    y = seq(0, 1, 0.25)
  )

  df |>
    areaspline_highcharts(
      x_var = "year",
      y_var = "y",
      proportion = TRUE,
      scale_percentage = TRUE
    ) |>
    expect_snapshot()

  # Custom fill_opacity
  df <- data.frame(
    year = 2020:2024,
    y = 1:5
  )

  res <- df |>
    areaspline_highcharts(
      x_var = "year",
      y_var = "y",
      fill_opacity = 0.3
    )

  expect_equal(res$plotOptions$areaspline$fillOpacity, 0.3)
  expect_equal(res$chart$type, "areaspline")

  # Default fill_opacity is 0.5
  res_default <- df |>
    areaspline_highcharts(
      x_var = "year",
      y_var = "y"
    )

  expect_equal(res_default$plotOptions$areaspline$fillOpacity, 0.5)

  # Stacking
  df <- data.frame(
    year = rep(2020:2024, each = 2),
    y = 1:10,
    group = rep(c("A", "B"), 5)
  )

  res_stacked <- df |>
    areaspline_highcharts(
      x_var = "year",
      y_var = "y",
      color_var = "group",
      stacking = "normal"
    )

  expect_equal(res_stacked$plotOptions$areaspline$stacking, "normal")

  res_pct <- df |>
    areaspline_highcharts(
      x_var = "year",
      y_var = "y",
      color_var = "group",
      stacking = "percent"
    )

  expect_equal(res_pct$plotOptions$areaspline$stacking, "percent")

  # No stacking by default
  res_no_stack <- df |>
    areaspline_highcharts(
      x_var = "year",
      y_var = "y",
      color_var = "group"
    )

  expect_null(res_no_stack$plotOptions$areaspline$stacking)

  # With other_vars and tooltip
  df <- data.frame(
    year = 2020:2024,
    prop = seq(0.1, 0.5, 0.1),
    n = c(10, 20, 30, 40, 50),
    total = 100
  )

  df |>
    areaspline_highcharts(
      x_var = "year",
      y_var = "prop",
      other_vars = list(
        "Täljare" = "n",
        "Nämnare" = "total"
      ),
      proportion = TRUE,
      scale_percentage = TRUE
    ) |>
    expect_snapshot()

  # Input validation: fill_opacity out of range
  expect_error(
    areaspline_highcharts(
      df,
      x_var = "year", y_var = "prop",
      fill_opacity = 1.5
    )
  )

  expect_error(
    areaspline_highcharts(
      df,
      x_var = "year", y_var = "prop",
      fill_opacity = -0.1
    )
  )

  # Input validation: invalid stacking value
  expect_error(
    areaspline_highcharts(
      df,
      x_var = "year", y_var = "prop",
      stacking = "invalid"
    )
  )

  # marker_enabled: default, disabled, and enable_on_hover
  res_marker_default <- areaspline_highcharts(
    df, x_var = "year", y_var = "prop"
  )
  expect_equal(
    res_marker_default$plotOptions$areaspline$marker, list(enabled = TRUE)
  )

  res_marker_disabled <- areaspline_highcharts(
    df,
    x_var = "year", y_var = "prop",
    marker_enabled = FALSE
  )
  expect_equal(
    res_marker_disabled$plotOptions$areaspline$marker, list(enabled = FALSE)
  )

  res_marker_hover <- areaspline_highcharts(
    df,
    x_var = "year", y_var = "prop",
    marker_enabled = "enable_on_hover",
    marker_size = 6
  )
  expect_equal(
    res_marker_hover$plotOptions$areaspline$marker,
    list(
      enabled = FALSE, states = list(hover = list(enabled = TRUE, radius = 6))
    )
  )

  expect_error(
    areaspline_highcharts(
      df,
      x_var = "year", y_var = "prop",
      marker_enabled = "invalid"
    )
  )
})

test_that("arrange_by works correctly", {
  df <- tibble::tribble(
    ~category, ~value,
    "A", 20,
    "B", 30,
    "C", 10
  )

  out <- bar_plot_highcharts(
    df = df,
    x_var = "category",
    y_var = "value",
    arrange_by = "value",
    arrange_desc = TRUE
  )

  expect_equal(out$xAxis$categories, I(c("B", "A", "C")))

  # Single series, data should follow category order
  series_data <- unlist(out$series[[1]]$data)
  expect_equal(series_data, c(30, 20, 10))

  df <- tibble::tribble(
    ~category, ~fill, ~value,
    "A", "x", 10,
    "A", "y", 20,
    "B", "x", 40,
    "B", "y", 60,
    "C", "x", 5,
    "C", "y", 15
  )

  out <- bar_plot_highcharts(
    df = df,
    x_var = "category",
    y_var = "value",
    fill_var = "fill",
    arrange_by = "value",
    arrange_desc = TRUE
  )

  expect_equal(out$xAxis$categories, I(c("B", "A", "C")))

  series_by_name <- purrr::set_names(
    out$series, purrr::map_chr(out$series, "name")
  )

  expect_equal(unlist(series_by_name[["x"]]$data), c(40, 10, 5))
  expect_equal(unlist(series_by_name[["y"]]$data), c(60, 20, 15))

  # Sanity: arrange_tmp should not leak into output
  expect_false("arrange_tmp" %in% names(df))

  df <- tibble::tribble(
    ~category, ~fill, ~value,
    "A", "x", 100,
    "A", "y", 20,
    "B", "x", 1,
    "B", "y", 60,
    "C", "x", 50,
    "C", "y", 15
  )

  out <- bar_plot_highcharts(
    df = df,
    x_var = "category",
    y_var = "value",
    fill_var = "fill",
    arrange_by = "value",
    arrange_by_fill = "y",
    arrange_desc = TRUE
  )

  # Sorted by value where fill is "y"
  expect_equal(out$xAxis$categories, I(c("B", "A", "C")))

  series_by_name <- purrr::set_names(
    out$series, purrr::map_chr(out$series, "name")
  )
  expect_equal(unlist(series_by_name[["y"]]$data), c(60, 20, 15))
  expect_equal(unlist(series_by_name[["x"]]$data), c(1, 100, 50))

  df <- tibble::tribble(
    ~category, ~value,
    "A", 20,
    "B", NA_real_,
    "C", 10
  )

  out <- bar_plot_highcharts(
    df = df,
    x_var = "category",
    y_var = "value",
    arrange_by = "value",
    arrange_desc = FALSE
  )

  expect_equal(out$xAxis$categories, I(c("B", "C", "A")))
})

test_that("add_total works", {
  df <- data.frame(
    x = c("A", "B", "C"),
    y = c(1, 2, 3),
    total = c(10, 20, 30)
  )

  df |>
    bar_plot_highcharts(
      x_var = "x",
      y_var = "y",
      horizontal = FALSE,
      add_total = TRUE,
      total_var = "total"
    ) |>
    expect_snapshot()

  df |>
    bar_plot_highcharts(
      x_var = "x",
      y_var = "y",
      horizontal = TRUE,
      add_total = TRUE,
      total_var = "total"
    ) |>
    expect_snapshot()

  df <- data.frame(
    x = factor(1:3, labels = letters[3:1]),
    y = 1:3
  )

  tmp <- df |>
    add_total_label(total_var = "y", x_var = "x")

  expect_type(levels(tmp$x), "character")

  tmp |>
    dplyr::pull("x") |>
    levels() |>
    stringr::str_remove(" \\(N=\\d+\\)") |>
    expect_equal(letters[3:1])

  df <- data.frame(
    x = factor(1:3, labels = letters[3:1]),
    total = 1:3,
    fill = rep(1:2, each = 3)
  )

  df |>
    add_total_label(total_var = "total", x_var = "x") |>
    dplyr::pull("x") |>
    levels() |>
    expect_equal(c("c (N=1)", "b (N=2)", "a (N=3)"))

})

test_that("set_size_params works", {

  df <- data.frame(
    x = c("A", "B", "A", "B"),
    y = c(1, 2, 3, 4),
    fill_var = c("Group 1", "Group 1", "Group 2", "group 2"),
    total = c(10, 20, 30, 40)
  )

  plt <- df |>
    bar_plot_highcharts(
      x_var = "x",
      y_var = "y",
      fill_var = "fill_var"
    )

  expect_equal(plt$plotOptions$column$groupPadding, 0.172)
  expect_equal(plt$plotOptions$column$pointPadding, 0.038)
  expect_equal(plt$plotOptions$series$pointWidth, 32)
  expect_equal(plt$chart$height, 650)
})

test_that("order_x_var works", {
  df <- data.frame(
    x = 1:3
  )

  result <- order_x_var(df, x_var = "x", order = c(2, 3, 1))
  expect_true(is.factor(result$x))
  expect_equal(levels(result$x), c("2", "3", "1"))
  expect_equal(as.character(result$x), c("2", "3", "1"))

  expect_error(
    order_x_var(df, x_var = "x", order = c(1, 2)),
    regexp = "is missing value present in x"
  )

  expect_warning(
    result_extra <- order_x_var(df, x_var = "x", order = c(1, 2, 3, 4)),
    regexp = "contains value not present in x"
  )
  expect_true(is.factor(result_extra$x))
  expect_equal(levels(result_extra$x), c("1", "2", "3", "4"))
  expect_equal(as.character(result_extra$x), c("1", "2", "3"))
})

test_that("forest_plot_highcharts works", {
  df <- data.frame(
    subgroup = c("A", "B", "C"),
    or = c(1.2, 0.8, 2.1),
    lower = c(0.9, 0.5, 1.3),
    upper = c(1.6, 1.1, 3.4),
    n = c(100, 80, 45)
  )

  forest_plot_highcharts(
    df = df,
    x_var = "subgroup",
    y_var = "or",
    y_lower = "lower",
    y_upper = "upper"
  ) |>
    expect_snapshot()

  forest_plot_highcharts(
    df = df,
    x_var = "subgroup",
    y_var = "or",
    y_lower = "lower",
    y_upper = "upper",
    reference_line = 1,
    log_scale = TRUE,
    other_vars = list(Observationer = "n"),
    arrange_by = "or"
  ) |>
    expect_snapshot()

  # errorbar and scatter series stay aligned by category after sorting
  res <- forest_plot_highcharts(
    df = df,
    x_var = "subgroup",
    y_var = "or",
    y_lower = "lower",
    y_upper = "upper",
    arrange_by = "or",
    n_decimals = 1
  )
  expect_equal(as.character(res$xAxis$categories), c("C", "A", "B"))
  ci_data <- purrr::map_dbl(res$series[[1]]$data, "low")
  point_data <- purrr::map_dbl(res$series[[2]]$data, "y")
  expect_equal(ci_data, c(1.3, 0.9, 0.5))
  expect_equal(point_data, c(2.1, 1.2, 0.8))

  # proportion works
  df <- data.frame(
    subgroup = c("A", "B", "C"),
    prop = c(0.4, 0.6, 0.25),
    lower = c(0.3, 0.5, 0.15),
    upper = c(0.5, 0.7, 0.35)
  )

  res <- forest_plot_highcharts(
    df = df,
    x_var = "subgroup",
    y_var = "prop",
    y_lower = "lower",
    y_upper = "upper",
    proportion = TRUE
  )

  expect_equal(res$yAxis$min, 0)
  expect_equal(res$yAxis$max, 100)
  expect_equal(res$yAxis$labels$format, "{value}%")

  ci_low <- purrr::map_dbl(res$series[[1]]$data, "low")
  ci_high <- purrr::map_dbl(res$series[[1]]$data, "high")
  point_data <- purrr::map_dbl(res$series[[2]]$data, "y")
  expect_equal(ci_low, c(30, 50, 15))
  expect_equal(ci_high, c(50, 70, 35))
  expect_equal(point_data, c(40, 60, 25))

  # scale_percentage = FALSE keeps the point estimate and CI on the 0-1 scale
  res_unscaled <- forest_plot_highcharts(
    df = df,
    x_var = "subgroup",
    y_var = "prop",
    y_lower = "lower",
    y_upper = "upper",
    proportion = TRUE,
    scale_percentage = FALSE,
    n_decimals = 2
  )

  ci_low_unscaled <- purrr::map_dbl(res_unscaled$series[[1]]$data, "low")
  ci_high_unscaled <- purrr::map_dbl(res_unscaled$series[[1]]$data, "high")
  point_data_unscaled <- purrr::map_dbl(res_unscaled$series[[2]]$data, "y")
  expect_equal(ci_low_unscaled, c(0.3, 0.5, 0.15))
  expect_equal(ci_high_unscaled, c(0.5, 0.7, 0.35))
  expect_equal(point_data_unscaled, c(0.4, 0.6, 0.25))

  expect_error(
    forest_plot_highcharts(
      df = df,
      x_var = "subgroup",
      y_var = "prop",
      y_lower = "lower",
      y_upper = "upper",
      proportion = TRUE,
      log_scale = TRUE
    )
  )

  # facet_by works
  df <- data.frame(
    subgroup = rep(c("A", "B"), 2),
    grp = rep(c("Male", "Female"), each = 2),
    or = c(1.2, 0.8, 1.0, 0.9),
    lower = c(0.9, 0.5, 0.7, 0.6),
    upper = c(1.6, 1.1, 1.4, 1.3)
  )

  res <- forest_plot_highcharts(
    df = df,
    x_var = "subgroup",
    y_var = "or",
    y_lower = "lower",
    y_upper = "upper",
    facet_by = "grp"
  )

  expect_length(res, 2)
  expect_snapshot(res)

  # color_var works
  df <- data.frame(
    subgroup = rep(c("A", "B", "C"), 2),
    grp = rep(c("Male", "Female"), each = 3),
    or = c(1.2, 0.8, 2.1, 1.0, 0.9, 1.8),
    lower = c(0.9, 0.5, 1.3, 0.7, 0.6, 1.1),
    upper = c(1.6, 1.1, 3.4, 1.4, 1.3, 2.9)
  )

  res <- forest_plot_highcharts(
    df = df,
    x_var = "subgroup",
    y_var = "or",
    y_lower = "lower",
    y_upper = "upper",
    color_var = "grp",
    legend_title = "Kön",
    n_decimals = 2
  )

  res |>
    expect_snapshot()

  # one point series and one (hidden from legend) errorbar series per group,
  # both sharing the same color so they read as belonging together
  expect_length(res$series, 4)
  expect_setequal(
    purrr::map_chr(res$series, "type"),
    c("errorbar", "scatter")
  )
  expect_equal(res$legend$title$text, "Kön")

  point_male <- purrr::keep(
    res$series, ~ .x$name == "Male" && .x$type == "scatter"
  )[[1]]
  ci_male <- purrr::keep(
    res$series, ~ .x$name == "Male" && .x$type == "errorbar"
  )[[1]]

  expect_null(point_male$showInLegend)
  expect_false(ci_male$showInLegend)
  expect_equal(point_male$color, ci_male$color)
  expect_equal(as.character(res$xAxis$categories), c("A", "B", "C"))
  expect_equal(purrr::map_dbl(point_male$data, "y"), c(1.2, 0.8, 2.1))
  expect_equal(purrr::map_dbl(ci_male$data, "low"), c(0.9, 0.5, 1.3))
  expect_equal(purrr::map_dbl(ci_male$data, "high"), c(1.6, 1.1, 3.4))

  # each group's point and CI share a pointPlacement offset so they don't
  # get drawn on top of each other within a shared x_var category
  expect_equal(point_male$pointPlacement, ci_male$pointPlacement)
  point_female <- purrr::keep(
    res$series, ~ .x$name == "Female" && .x$type == "scatter"
  )[[1]]
  expect_true(point_male$pointPlacement != point_female$pointPlacement)

  # an incomplete color_var/x_var grid is filled in as a gap rather than
  # silently misaligning the two series
  res_missing <- forest_plot_highcharts(
    df = df[-6, ],
    x_var = "subgroup",
    y_var = "or",
    y_lower = "lower",
    y_upper = "upper",
    color_var = "grp"
  )
  point_female <- purrr::keep(
    res_missing$series, ~ .x$name == "Female" && .x$type == "scatter"
  )[[1]]
  expect_length(point_female$data, 3)
  expect_null(point_female$data[[3]]$y)
})
