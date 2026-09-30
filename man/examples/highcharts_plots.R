## Bar plot
df <- data.frame(
  unit = paste0("Enhet ", 1:10),
  y = stats::rnorm(10, mean = 50)
)

#Basic bar plot
bar_plot_highcharts(
  df,
  x_var = "unit",
  y_var = "y",
  horizontal = TRUE,
  y_lim = c(0, 100)
) |> view_highcharts()

#Add horizontal line
bar_plot_highcharts(
  df,
  x_var = "unit",
  y_var = "y",
  horizontal = TRUE,
  y_lim = c(0, 100),
  horizontal_line = 50
) |> view_highcharts()

#Arrange by value
bar_plot_highcharts(
  df,
  x_var = "unit",
  y_var = "y",
  horizontal = TRUE,
  y_lim = c(0, 100),
  horizontal_line = 50,
  arrange_by = "y",
  arrange_desc = TRUE
) |> view_highcharts()

##Line plot
df <- data.frame(
  year = rep(2015:2025, 2),
  ind_age = c(stats::rnorm(11, mean = 50),
        stats::rnorm(11, mean = 40, sd = 5)),
  ind_diabetic = c(stats::runif(11, min = 0.7, max = 1),
             stats::runif(11, min = 0.4, max = 1)),
  grp = c(rep("A", 11), rep("B", 11))
)

#Basic line plot with two groups
line_plot_highcharts(
  df,
  x_var = "year",
  y_var = "ind_age",
  color_var = "grp",
  y_lim = c(0, 80)
) |> view_highcharts()

#Basic line plot with proportion
line_plot_highcharts(
  df,
  x_var = "year",
  y_var = "ind_diabetic",
  color_var = "grp",
  proportion = TRUE
) |> view_highcharts()

#Line plot with horizontal line
line_plot_highcharts(
  df,
  x_var = "year",
  y_var = "ind_age",
  color_var = "grp",
  y_lim = c(0, 80),
  horizontal_line = 40,
  y_lab = "Medelålder"
) |> view_highcharts()

##Survial plots
df <- data.frame(
  estimate = stats::rbinom(2* 365, size = 1, prob = 0.1)
) |>
  dplyr::mutate(estimate = 1 - cumsum(estimate) / dplyr::n(),
                time = dplyr::row_number())

#Basic survival plot
line_plot_highcharts(
  df,
  x_var = "time",
  y_var = "estimate",
  proportion = TRUE,
  surv = TRUE,
  y_lim = c(50, 100)
) |> view_highcharts()

#Change labels on x axis to years
line_plot_highcharts(
  df,
  x_var = "time",
  y_var = "estimate",
  proportion = TRUE,
  surv = TRUE,
  minify_step_curve = FALSE,
  y_lim = c(50, 100),
  x_labels_surv_type = "years",
  x_lab = "År efter operation",
  y_lab = "Överlevnad"
) |> view_highcharts()
