test_that("make_series works", {
  df <- data.frame(
    year = 2015:2024,
    n = 1:10,
    county = "VGR"
  )

  make_series(df, list(y = "n")) |>
    expect_equal(
      list(
        list(
          data = I(as.list(1:10)),
          name = "",
          color = "#116875"
        )
      )
    )

  make_series(df, list(y = "n"), "county") |>
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

  make_series(df, list(y = "n"), "county") |>
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

  make_series(df, list(y = "value"), c("county", "type")) |>
    expect_equal(
      list(
        list(data = I(as.list(2)),
             name = "Stockholm, value 1",
             color = "#116875"),
        list(data = I(as.list(4)),
             name = "Stockholm, value 2",
             color = "#FC5930"),
        list(data = I(as.list(1)),
             name = "VGR, value 1",
             color = "#6F45BB"),
        list(data = I(as.list(3)),
             name = "VGR, value 2",
             color = "#89163B")
      )
    )

  df <- data.frame(
    year = 2023:2024,
    y = 100
  )

  make_series(df, list(y = "y"), "year", palette_type = "sequential_1") |>
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
    other_vars = list(Täljare = "n", Nämnare = "total")
  ) |>
    expect_equal(
      list(
        list(
          data = list(
            list(y = 0.5, n = 50, total = 100),
            list(y = 1, n = 100, total = 100)
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
    scale_percentage = TRUE
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

  make_series(df, list(y = "prop"), colors = "#6F45BB") |>
    expect_equal(
      list(
        list(
          data = I(as.list(c(0.5, 1))),
          name = "",
          color = "#6F45BB"
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
    group_var_order = "auto_numeric"
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
    other_vars = NULL,
    legend_title = "abc"
  ) |>
    expect_equal(
      list(
        title = list(text = ""),
        chart = list(
          type = "column",
          inverted = FALSE,
          height = "80%"
        ),
        xAxis = list(categories = structure("a", class = "AsIs")),
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
          title = list(text = "abc")
        ),
        yAxis = list(labels = list(format = "{value}")),
        tooltip = list(pointFormat = "<b>{point.y}</b>")
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
    dplyr::mutate(obfuscated_reason = dplyr::if_else(x == "b" & y == 5,
                                                     "N < 15",
                                                     NA),
                  y = dplyr::if_else(x == "b" & y == 5, NA, y))

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
      color_var = c("county", "group")
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
    facet_by = "type"
  )

  expect_snapshot(res)

})
