# bar_plot_highcharts works

    Code
      bar_plot_highcharts(df, x_var = "x", y_var = "y", fill_var = "color", position = "stack",
        y_lim = c(0, 50), proportion = FALSE)
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "column"
      
      
      $xAxis
      $xAxis$categories
      [1] "a" "b" "c"
      
      
      $series
      [[1]]
      [[1]]$data
      [1] 1 4 7
      
      [[1]]$name
      [1] "A4"
      
      [[1]]$color
      [1] "#0072B2"
      
      
      [[2]]
      [[2]]$data
      [1] 2 5 8
      
      [[2]]$name
      [1] "B5"
      
      [[2]]$color
      [1] "#D55E00"
      
      
      [[3]]
      [[3]]$data
      [1] 3 6 9
      
      [[3]]$name
      [1] "C6"
      
      [[3]]$color
      [1] "#CC79A7"
      
      
      
      $yAxis
      $yAxis$min
      [1] 0
      
      $yAxis$max
      [1] 50
      
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}: <b>{point.y}</b>"
      
      
      $plotOptions
      $plotOptions$column
      $plotOptions$column$stacking
      [1] "normal"
      
      
      

---

    Code
      bar_plot_highcharts(df, x_var = "x", y_var = "y", fill_var = "color", position = "dodge",
        y_breaks = c(0, 10, 50))
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "column"
      
      
      $xAxis
      $xAxis$categories
      [1] "a" "b" "c"
      
      
      $series
      [[1]]
      [[1]]$data
      [1] 1 4 7
      
      [[1]]$name
      [1] "A4"
      
      [[1]]$color
      [1] "#0072B2"
      
      
      [[2]]
      [[2]]$data
      [1] 2 5 8
      
      [[2]]$name
      [1] "B5"
      
      [[2]]$color
      [1] "#D55E00"
      
      
      [[3]]
      [[3]]$data
      [1] 3 6 9
      
      [[3]]$name
      [1] "C6"
      
      [[3]]$color
      [1] "#CC79A7"
      
      
      
      $yAxis
      $yAxis$tickPositions
      [1]  0 10 50
      
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}: <b>{point.y}</b>"
      
      

---

    Code
      bar_plot_highcharts(df, x_var = "x", y_var = "y")
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "column"
      
      
      $xAxis
      $xAxis$categories
      [1] "a" "b" "c"
      
      
      $series
      [[1]]
      [[1]]$data
      [1] 1 2 3
      
      [[1]]$name
      [1] ""
      
      [[1]]$color
      [1] "#0072B2"
      
      
      
      $legend
      $legend$enabled
      [1] FALSE
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "<b>{point.y}</b>"
      
      

---

    Code
      bar_plot_highcharts(df, x_var = "x", y_var = "y", proportion = TRUE,
        scale_percentage = TRUE)
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "column"
      
      
      $xAxis
      $xAxis$categories
      [1] "a" "b" "c" "d" "e"
      
      
      $series
      [[1]]
      [[1]]$data
      [1]   0  25  50  75 100
      
      [[1]]$name
      [1] ""
      
      [[1]]$color
      [1] "#0072B2"
      
      
      
      $legend
      $legend$enabled
      [1] FALSE
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}%"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "<b>{point.y}%</b>"
      
      

---

    Code
      bar_plot_highcharts(df, x_var = "year", y_var = "y", fill_var = "county")
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "column"
      
      
      $xAxis
      $xAxis$categories
       [1] "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019"
      [11] "2020" "2021" "2022" "2023" "2024" "2025"
      
      
      $series
      [[1]]
      [[1]]$data
       [1] 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
      
      [[1]]$name
      [1] "Stockholm"
      
      [[1]]$color
      [1] "#0072B2"
      
      
      [[2]]
      [[2]]$data
       [1] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
      
      [[2]]$name
      [1] "VGR"
      
      [[2]]$color
      [1] "#D55E00"
      
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}: <b>{point.y}</b>"
      
      

---

    Code
      bar_plot_highcharts(df, x_var = "year", y_var = "y", fill_var = "county",
        horizontal = TRUE, proportion = TRUE, scale_percentage = FALSE, y_lim = c(0,
          10))
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "column"
      
      $chart$inverted
      [1] TRUE
      
      
      $xAxis
      $xAxis$categories
       [1] "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019"
      [11] "2020" "2021" "2022" "2023" "2024" "2025"
      
      
      $series
      [[1]]
      [[1]]$data
       [1] 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
      
      [[1]]$name
      [1] "Stockholm"
      
      [[1]]$color
      [1] "#0072B2"
      
      
      [[2]]
      [[2]]$data
       [1] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
      
      [[2]]$name
      [1] "VGR"
      
      [[2]]$color
      [1] "#D55E00"
      
      
      
      $yAxis
      $yAxis$min
      [1] 0
      
      $yAxis$max
      [1] 10
      
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}%"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}: <b>{point.y}%</b>"
      
      

---

    Code
      bar_plot_highcharts(df, x_var = "county", y_var = "prop", other_vars = list(
        Nämnare = "total", Täljare = "n"), proportion = TRUE, scale_percentage = TRUE)
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "column"
      
      
      $xAxis
      $xAxis$categories
      [1] "a" "b"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]$data[[1]]
      [[1]]$data[[1]]$y
      [1] 50
      
      [[1]]$data[[1]]$total
      [1] 10
      
      [[1]]$data[[1]]$n
      [1] 5
      
      
      [[1]]$data[[2]]
      [[1]]$data[[2]]$y
      [1] 100
      
      [[1]]$data[[2]]$total
      [1] 10
      
      [[1]]$data[[2]]$n
      [1] 10
      
      
      
      [[1]]$name
      [1] ""
      
      [[1]]$color
      [1] "#0072B2"
      
      
      
      $legend
      $legend$enabled
      [1] FALSE
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}%"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "<b>{point.y}%</b><br>Nämnare: <b>{point.total}</b><br>Täljare: <b>{point.n}</b>"
      
      

---

    Code
      bar_plot_highcharts(df, x_var = "county", y_var = "prop", fill_var = "year",
        other_vars = list(Nämnare = "total", Täljare = "n"), proportion = TRUE,
        scale_percentage = TRUE, x_lab = "Hello World!")
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "column"
      
      
      $xAxis
      $xAxis$categories
      [1] "a" "b"
      
      $xAxis$title
      $xAxis$title$text
      [1] "Hello World!"
      
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]$data[[1]]
      [[1]]$data[[1]]$y
      [1] 50
      
      [[1]]$data[[1]]$total
      [1] 10
      
      [[1]]$data[[1]]$n
      [1] 5
      
      
      [[1]]$data[[2]]
      [[1]]$data[[2]]$y
      [1] 100
      
      [[1]]$data[[2]]$total
      [1] 10
      
      [[1]]$data[[2]]$n
      [1] 10
      
      
      
      [[1]]$name
      [1] "2023"
      
      [[1]]$color
      [1] "#0072B2"
      
      
      [[2]]
      [[2]]$data
      [[2]]$data[[1]]
      [[2]]$data[[1]]$y
      [1] 50
      
      [[2]]$data[[1]]$total
      [1] 10
      
      [[2]]$data[[1]]$n
      [1] 5
      
      
      [[2]]$data[[2]]
      [[2]]$data[[2]]$y
      [1] 100
      
      [[2]]$data[[2]]$total
      [1] 10
      
      [[2]]$data[[2]]$n
      [1] 10
      
      
      
      [[2]]$name
      [1] "2024"
      
      [[2]]$color
      [1] "#D55E00"
      
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}%"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}: <b>{point.y}%</b><br>Nämnare: <b>{point.total}</b><br>Täljare: <b>{point.n}</b>"
      
      

# line_plot_highcharts works

    Code
      line_plot_highcharts(df, x_var = "year", y_var = "y")
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "line"
      
      
      $xAxis
      $xAxis$categories
       [1] "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019"
      [11] "2020" "2021" "2022" "2023" "2024" "2025"
      
      
      $series
      [[1]]
      [[1]]$data
       [1] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
      
      [[1]]$name
      [1] ""
      
      [[1]]$color
      [1] "#0072B2"
      
      
      
      $legend
      $legend$enabled
      [1] FALSE
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "<b>{point.y}</b>"
      
      

---

    Code
      line_plot_highcharts(df, x_var = "year", y_var = "y", color_var = "county")
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "line"
      
      
      $xAxis
      $xAxis$categories
       [1] "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019"
      [11] "2020" "2021" "2022" "2023" "2024" "2025"
      
      
      $series
      [[1]]
      [[1]]$data
       [1] 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10 10
      
      [[1]]$name
      [1] "Stockholm"
      
      [[1]]$color
      [1] "#0072B2"
      
      
      [[2]]
      [[2]]$data
       [1] 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5 5
      
      [[2]]$name
      [1] "VGR"
      
      [[2]]$color
      [1] "#D55E00"
      
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}: <b>{point.y}</b>"
      
      

# box_plot_highcharts work

    Code
      box_plot_highcharts(df = df, x_var = "x", y_var = "median", y_min = "low",
        y_lower = "q1", y_upper = "q3", y_max = "high")
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "boxplot"
      
      
      $xAxis
      $xAxis$categories
      [1] "a" "b"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]$data[[1]]
      [[1]]$data[[1]]$median
      [1] 3
      
      [[1]]$data[[1]]$low
      [1] 1
      
      [[1]]$data[[1]]$q1
      [1] 2
      
      [[1]]$data[[1]]$q3
      [1] 4
      
      [[1]]$data[[1]]$high
      [1] 5
      
      
      [[1]]$data[[2]]
      [[1]]$data[[2]]$median
      [1] 3
      
      [[1]]$data[[2]]$low
      [1] 1
      
      [[1]]$data[[2]]$q1
      [1] 2
      
      [[1]]$data[[2]]$q3
      [1] 4
      
      [[1]]$data[[2]]$high
      [1] 5
      
      
      
      [[1]]$name
      [1] ""
      
      [[1]]$color
      [1] "#0072B2"
      
      
      
      $legend
      $legend$enabled
      [1] FALSE
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "\n      95e kvantilen: <b>{point.high}</b><br>\n      75e kvantilen: <b>{point.q3}</b><br>\n      Median: <b>{point.median}</b><br>\n      25e kvantilen: <b>{point.q1}</b><br>\n      5e kvantilen: <b>{point.low}</b><br>"
      
      

---

    Code
      box_plot_highcharts(df = df, x_var = "x", y_var = "median", y_min = "low",
        y_lower = "q1", y_upper = "q3", y_max = "high", fill_var = "x", other_vars = list(
          Observationer = "total"), horizontal = TRUE)
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "boxplot"
      
      $chart$inverted
      [1] TRUE
      
      
      $xAxis
      $xAxis$categories
      [1] "a" "b"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]$data[[1]]
      [[1]]$data[[1]]$median
      [1] 3
      
      [[1]]$data[[1]]$low
      [1] 1
      
      [[1]]$data[[1]]$q1
      [1] 2
      
      [[1]]$data[[1]]$q3
      [1] 4
      
      [[1]]$data[[1]]$high
      [1] 5
      
      [[1]]$data[[1]]$total
      [1] 10
      
      
      
      [[1]]$name
      [1] "a"
      
      [[1]]$color
      [1] "#0072B2"
      
      
      [[2]]
      [[2]]$data
      [[2]]$data[[1]]
      [[2]]$data[[1]]$median
      [1] 3
      
      [[2]]$data[[1]]$low
      [1] 1
      
      [[2]]$data[[1]]$q1
      [1] 2
      
      [[2]]$data[[1]]$q3
      [1] 4
      
      [[2]]$data[[1]]$high
      [1] 5
      
      [[2]]$data[[1]]$total
      [1] 10
      
      
      
      [[2]]$name
      [1] "b"
      
      [[2]]$color
      [1] "#D55E00"
      
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}<br>\n      95e kvantilen: <b>{point.high}</b><br>\n      75e kvantilen: <b>{point.q3}</b><br>\n      Median: <b>{point.median}</b><br>\n      25e kvantilen: <b>{point.q1}</b><br>\n      5e kvantilen: <b>{point.low}</b><br><br>Observationer: <b>{point.total}</b>"
      
      

