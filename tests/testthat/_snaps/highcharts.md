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
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "100%"
      
      
      $xAxis
      $xAxis$categories
      [1] "a" "b" "c"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]
      [1] 1
      
      [[2]]
      [1] 4
      
      [[3]]
      [1] 7
      
      
      [[1]]$name
      [1] "A4"
      
      [[1]]$color
      [1] "#116875"
      
      
      [[2]]
      [[2]]$data
      [[1]]
      [1] 2
      
      [[2]]
      [1] 5
      
      [[3]]
      [1] 8
      
      
      [[2]]$name
      [1] "B5"
      
      [[2]]$color
      [1] "#FC5930"
      
      
      [[3]]
      [[3]]$data
      [[1]]
      [1] 3
      
      [[2]]
      [1] 6
      
      [[3]]
      [1] 9
      
      
      [[3]]$name
      [1] "C6"
      
      [[3]]$color
      [1] "#6F45BB"
      
      
      
      $legend
      $legend$reversed
      [1] TRUE
      
      
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
      
      
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] " "
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

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
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "100%"
      
      
      $xAxis
      $xAxis$categories
      [1] "a" "b" "c"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]
      [1] 1
      
      [[2]]
      [1] 4
      
      [[3]]
      [1] 7
      
      
      [[1]]$name
      [1] "A4"
      
      [[1]]$color
      [1] "#116875"
      
      
      [[2]]
      [[2]]$data
      [[1]]
      [1] 2
      
      [[2]]
      [1] 5
      
      [[3]]
      [1] 8
      
      
      [[2]]$name
      [1] "B5"
      
      [[2]]$color
      [1] "#FC5930"
      
      
      [[3]]
      [[3]]$data
      [[1]]
      [1] 3
      
      [[2]]
      [1] 6
      
      [[3]]
      [1] 9
      
      
      [[3]]$name
      [1] "C6"
      
      [[3]]$color
      [1] "#6F45BB"
      
      
      
      $legend
      $legend$reversed
      [1] TRUE
      
      
      $yAxis
      $yAxis$tickPositions
      [1]  0 10 50
      
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}: <b>{point.y}</b>"
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] " "
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

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
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "100%"
      
      
      $xAxis
      $xAxis$categories
      [1] "a" "b" "c"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]
      [1] 1
      
      [[2]]
      [1] 2
      
      [[3]]
      [1] 3
      
      
      [[1]]$name
      [1] ""
      
      [[1]]$color
      [1] "#116875"
      
      
      
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
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] " "
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

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
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "100%"
      
      
      $xAxis
      $xAxis$categories
      [1] "a" "b" "c" "d" "e"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]
      [1] 0
      
      [[2]]
      [1] 25
      
      [[3]]
      [1] 50
      
      [[4]]
      [1] 75
      
      [[5]]
      [1] 100
      
      
      [[1]]$name
      [1] ""
      
      [[1]]$color
      [1] "#116875"
      
      
      
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
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] " "
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

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
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "100%"
      
      
      $xAxis
      $xAxis$categories
       [1] "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019"
      [11] "2020" "2021" "2022" "2023" "2024" "2025"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]
      [1] 10
      
      [[2]]
      [1] 10
      
      [[3]]
      [1] 10
      
      [[4]]
      [1] 10
      
      [[5]]
      [1] 10
      
      [[6]]
      [1] 10
      
      [[7]]
      [1] 10
      
      [[8]]
      [1] 10
      
      [[9]]
      [1] 10
      
      [[10]]
      [1] 10
      
      [[11]]
      [1] 10
      
      [[12]]
      [1] 10
      
      [[13]]
      [1] 10
      
      [[14]]
      [1] 10
      
      [[15]]
      [1] 10
      
      [[16]]
      [1] 10
      
      
      [[1]]$name
      [1] "Stockholm"
      
      [[1]]$color
      [1] "#116875"
      
      
      [[2]]
      [[2]]$data
      [[1]]
      [1] 5
      
      [[2]]
      [1] 5
      
      [[3]]
      [1] 5
      
      [[4]]
      [1] 5
      
      [[5]]
      [1] 5
      
      [[6]]
      [1] 5
      
      [[7]]
      [1] 5
      
      [[8]]
      [1] 5
      
      [[9]]
      [1] 5
      
      [[10]]
      [1] 5
      
      [[11]]
      [1] 5
      
      [[12]]
      [1] 5
      
      [[13]]
      [1] 5
      
      [[14]]
      [1] 5
      
      [[15]]
      [1] 5
      
      [[16]]
      [1] 5
      
      
      [[2]]$name
      [1] "VGR"
      
      [[2]]$color
      [1] "#FC5930"
      
      
      
      $legend
      $legend$reversed
      [1] TRUE
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}: <b>{point.y}</b>"
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] " "
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

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
      
      $chart$height
      [1] "100%"
      
      
      $xAxis
      $xAxis$categories
       [1] "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019"
      [11] "2020" "2021" "2022" "2023" "2024" "2025"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]
      [1] 10
      
      [[2]]
      [1] 10
      
      [[3]]
      [1] 10
      
      [[4]]
      [1] 10
      
      [[5]]
      [1] 10
      
      [[6]]
      [1] 10
      
      [[7]]
      [1] 10
      
      [[8]]
      [1] 10
      
      [[9]]
      [1] 10
      
      [[10]]
      [1] 10
      
      [[11]]
      [1] 10
      
      [[12]]
      [1] 10
      
      [[13]]
      [1] 10
      
      [[14]]
      [1] 10
      
      [[15]]
      [1] 10
      
      [[16]]
      [1] 10
      
      
      [[1]]$name
      [1] "Stockholm"
      
      [[1]]$color
      [1] "#116875"
      
      
      [[2]]
      [[2]]$data
      [[1]]
      [1] 5
      
      [[2]]
      [1] 5
      
      [[3]]
      [1] 5
      
      [[4]]
      [1] 5
      
      [[5]]
      [1] 5
      
      [[6]]
      [1] 5
      
      [[7]]
      [1] 5
      
      [[8]]
      [1] 5
      
      [[9]]
      [1] 5
      
      [[10]]
      [1] 5
      
      [[11]]
      [1] 5
      
      [[12]]
      [1] 5
      
      [[13]]
      [1] 5
      
      [[14]]
      [1] 5
      
      [[15]]
      [1] 5
      
      [[16]]
      [1] 5
      
      
      [[2]]$name
      [1] "VGR"
      
      [[2]]$color
      [1] "#FC5930"
      
      
      
      $legend
      $legend$reversed
      [1] TRUE
      
      
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
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] " "
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

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
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "100%"
      
      
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
      [1] "#116875"
      
      
      
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
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] " "
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

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
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "100%"
      
      
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
      [1] "#116875"
      
      
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
      [1] "#FC5930"
      
      
      
      $legend
      $legend$reversed
      [1] TRUE
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}%"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}: <b>{point.y}%</b><br>Nämnare: <b>{point.total}</b><br>Täljare: <b>{point.n}</b>"
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] " "
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

---

    Code
      bar_plot_highcharts(df, x_var = "county", y_var = "prop", fill_var = "year",
        other_vars = list(Nämnare = "total", Täljare = "n"), proportion = TRUE,
        scale_percentage = TRUE, fill_var_order = c(2023, 2024))
    Condition
      Warning:
      There was 1 warning in `dplyr::mutate()`.
      i In argument: `dplyr::across(.data$series_var, ~factor(.x, levels = group_var_order))`.
      Caused by warning:
      ! Use of .data in tidyselect expressions was deprecated in tidyselect 1.2.0.
      i Please use `"series_var"` instead of `.data$series_var`
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "column"
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "100%"
      
      
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
      [1] 2023
      Levels: 2023 2024
      
      [[1]]$color
      [1] "#116875"
      
      
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
      [1] 2024
      Levels: 2023 2024
      
      [[2]]$color
      [1] "#FC5930"
      
      
      
      $legend
      $legend$reversed
      [1] TRUE
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}%"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}: <b>{point.y}%</b><br>Nämnare: <b>{point.total}</b><br>Täljare: <b>{point.n}</b>"
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] " "
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

---

    Code
      bar_plot_highcharts(df, x_var = "enhet", y_var = "y", arrange_by = "y")
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "column"
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "100%"
      
      
      $xAxis
      $xAxis$categories
       [1] "Enhet 10" "Enhet 9"  "Enhet 8"  "Enhet 7"  "Enhet 6"  "Enhet 5" 
       [7] "Enhet 4"  "Enhet 3"  "Enhet 2"  "Enhet 1" 
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]
      [1] 10
      
      [[2]]
      [1] 9
      
      [[3]]
      [1] 8
      
      [[4]]
      [1] 7
      
      [[5]]
      [1] 6
      
      [[6]]
      [1] 5
      
      [[7]]
      [1] 4
      
      [[8]]
      [1] 3
      
      [[9]]
      [1] 2
      
      [[10]]
      [1] 1
      
      
      [[1]]$name
      [1] ""
      
      [[1]]$color
      [1] "#116875"
      
      
      
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
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] " "
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

---

    Code
      bar_plot_highcharts(df, x_var = "enhet", y_var = "y", arrange_by = "y",
        arrange_desc = FALSE)
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "column"
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "100%"
      
      
      $xAxis
      $xAxis$categories
       [1] "Enhet 1"  "Enhet 2"  "Enhet 3"  "Enhet 4"  "Enhet 5"  "Enhet 6" 
       [7] "Enhet 7"  "Enhet 8"  "Enhet 9"  "Enhet 10"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]
      [1] 1
      
      [[2]]
      [1] 2
      
      [[3]]
      [1] 3
      
      [[4]]
      [1] 4
      
      [[5]]
      [1] 5
      
      [[6]]
      [1] 6
      
      [[7]]
      [1] 7
      
      [[8]]
      [1] 8
      
      [[9]]
      [1] 9
      
      [[10]]
      [1] 10
      
      
      [[1]]$name
      [1] ""
      
      [[1]]$color
      [1] "#116875"
      
      
      
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
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] " "
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

---

    Code
      bar_plot_highcharts(df, x_var = "enhet", y_var = "y", arrange_by = "y",
        arrange_desc = FALSE, horizontal = TRUE, color_x_value = list(`Enhet 3` = "#6F45BB"))
    Output
      $title
      $title$text
      [1] ""
      
      
      $chart
      $chart$type
      [1] "column"
      
      $chart$inverted
      [1] TRUE
      
      $chart$height
      [1] "100%"
      
      
      $xAxis
      $xAxis$categories
       [1] "Enhet 1"  "Enhet 2"  "Enhet 3"  "Enhet 4"  "Enhet 5"  "Enhet 6" 
       [7] "Enhet 7"  "Enhet 8"  "Enhet 9"  "Enhet 10"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]
      [1] 1
      
      [[2]]
      [1] 2
      
      [[3]]
      [[3]]$y
      [1] 3
      
      [[3]]$color
      [1] "#6F45BB"
      
      
      [[4]]
      [1] 4
      
      [[5]]
      [1] 5
      
      [[6]]
      [1] 6
      
      [[7]]
      [1] 7
      
      [[8]]
      [1] 8
      
      [[9]]
      [1] 9
      
      [[10]]
      [1] 10
      
      
      [[1]]$name
      [1] ""
      
      [[1]]$color
      [1] "#116875"
      
      
      
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
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] " "
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

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
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "100%"
      
      
      $xAxis
      $xAxis$categories
      [1] "a"  "b"  "b*" "c" 
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]
      [1] 1
      
      [[2]]
      [1] 2
      
      [[3]]
      [1] 3
      
      [[4]]
      [1] 4
      
      [[5]]
      NULL
      
      [[6]]
      [1] 6
      
      [[7]]
      [1] 7
      
      [[8]]
      [1] 8
      
      [[9]]
      [1] 9
      
      
      [[1]]$name
      [1] ""
      
      [[1]]$color
      [1] "#116875"
      
      
      
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
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 15
      
      
      
      $caption
      $caption$text
      [1] "* Indikerar att data inte kan visas p.g.a. risk för röjande<br/>eller för lite data."
      
      $caption$align
      [1] "left"
      
      $caption$style
      $caption$style$fontSize
      [1] "12px"
      
      
      

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
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "80%"
      
      
      $xAxis
      $xAxis$categories
       [1] "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019"
      [11] "2020" "2021" "2022" "2023" "2024" "2025"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]
      [1] 5
      
      [[2]]
      [1] 5
      
      [[3]]
      [1] 5
      
      [[4]]
      [1] 5
      
      [[5]]
      [1] 5
      
      [[6]]
      [1] 5
      
      [[7]]
      [1] 5
      
      [[8]]
      [1] 5
      
      [[9]]
      [1] 5
      
      [[10]]
      [1] 5
      
      [[11]]
      [1] 5
      
      [[12]]
      [1] 5
      
      [[13]]
      [1] 5
      
      [[14]]
      [1] 5
      
      [[15]]
      [1] 5
      
      [[16]]
      [1] 5
      
      
      [[1]]$name
      [1] ""
      
      [[1]]$color
      [1] "#116875"
      
      
      
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
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 8
      
      
      

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
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "80%"
      
      
      $xAxis
      $xAxis$categories
       [1] "2010" "2011" "2012" "2013" "2014" "2015" "2016" "2017" "2018" "2019"
      [11] "2020" "2021" "2022" "2023" "2024" "2025"
      
      
      $series
      [[1]]
      [[1]]$data
      [[1]]
      [1] 10
      
      [[2]]
      [1] 10
      
      [[3]]
      [1] 10
      
      [[4]]
      [1] 10
      
      [[5]]
      [1] 10
      
      [[6]]
      [1] 10
      
      [[7]]
      [1] 10
      
      [[8]]
      [1] 10
      
      [[9]]
      [1] 10
      
      [[10]]
      [1] 10
      
      [[11]]
      [1] 10
      
      [[12]]
      [1] 10
      
      [[13]]
      [1] 10
      
      [[14]]
      [1] 10
      
      [[15]]
      [1] 10
      
      [[16]]
      [1] 10
      
      
      [[1]]$name
      [1] "Stockholm"
      
      [[1]]$color
      [1] "#116875"
      
      
      [[2]]
      [[2]]$data
      [[1]]
      [1] 5
      
      [[2]]
      [1] 5
      
      [[3]]
      [1] 5
      
      [[4]]
      [1] 5
      
      [[5]]
      [1] 5
      
      [[6]]
      [1] 5
      
      [[7]]
      [1] 5
      
      [[8]]
      [1] 5
      
      [[9]]
      [1] 5
      
      [[10]]
      [1] 5
      
      [[11]]
      [1] 5
      
      [[12]]
      [1] 5
      
      [[13]]
      [1] 5
      
      [[14]]
      [1] 5
      
      [[15]]
      [1] 5
      
      [[16]]
      [1] 5
      
      
      [[2]]$name
      [1] "VGR"
      
      [[2]]$color
      [1] "#FC5930"
      
      
      
      $legend
      $legend$reversed
      [1] TRUE
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}: <b>{point.y}</b>"
      
      
      $plotOptions
      $plotOptions$series
      $plotOptions$series$pointWidth
      [1] 8
      
      
      

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
      
      $chart$inverted
      [1] FALSE
      
      $chart$height
      [1] "80%"
      
      
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
      [1] "#116875"
      
      
      
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
      
      $chart$height
      [1] "80%"
      
      
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
      [1] "#116875"
      
      
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
      [1] "#FC5930"
      
      
      
      $legend
      $legend$reversed
      [1] TRUE
      
      
      $yAxis
      $yAxis$labels
      $yAxis$labels$format
      [1] "{value}"
      
      
      
      $tooltip
      $tooltip$pointFormat
      [1] "{series.name}<br>\n      95e kvantilen: <b>{point.high}</b><br>\n      75e kvantilen: <b>{point.q3}</b><br>\n      Median: <b>{point.median}</b><br>\n      25e kvantilen: <b>{point.q1}</b><br>\n      5e kvantilen: <b>{point.low}</b><br><br>Observationer: <b>{point.total}</b>"
      
      

