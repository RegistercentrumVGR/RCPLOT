fit <- survival::survfit(
  survival::Surv(time / 365.24, status) ~ sex,
  survival::colon
)

km_plot_2(fit)

km_plot_2(
  survfit_obj = fit,
  add_ci = FALSE,
  x_lab = "time",
  y_lab = "surv",
  legend_title = "legend title",
  legend_labs = c("man", "woman"),
  legend_position = "right"
)

km_plot_2(
  fit,
  plot_type = "cumhaz"
)

fit <- survival::survfit(
  survival::Surv(time / 365.24, status) ~ sex + rx,
  survival::colon
)

km_plot_2(
  fit,
  facet_by = "rx"
)

km_plot_2(
  fit
)

km_plot_2(
  fit,
  facet_by = "rx",
  risk_table = TRUE,
  plotly = TRUE
)

fit_3 <- survival::survfit(
  survival::Surv(time / 365.24, status) ~ 1,
  survival::colon
)

km_plot_2(fit_3)
