fit <- survival::survfit(survival::Surv(time, status) ~ sex, data = survival::lung)

surv_df <- summary(fit, times = sort(unique(fit$time)))

surv_data <- tibble::tibble(
  strata  = as.character(surv_df$strata),
  year    = surv_df$time,
  surv    = surv_df$surv,
  lower   = surv_df$lower,
  upper   = surv_df$upper,
  n.risk  = surv_df$n.risk,
  n.event = surv_df$n.event
) |>
  dplyr::mutate(
    strata = strata |>
      strsplit(", ") |>
      lapply(gsub, pattern = ".*=", replacement = "") |>
      lapply(paste0, collapse = ", ") |>
      unlist()
  ) |>
  dplyr::arrange(strata, year)

surv_data

line_plot_highcharts(
  df = surv_data,
  x_var = "year",
  y_var = "surv",
  color_var = "strata",
  surv = T,
  proportion = T
) |>
  export_highcharts()

library(ggplot2)

ggplot(surv_data, aes(x = year, y = surv, color = strata)) +
  geom_step()
