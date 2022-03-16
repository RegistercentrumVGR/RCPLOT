test_that("saving png, tiff and pdf works", {
  scip_on_os(os = "mac")
  op <- options()

  plt <-
    ggplot2::ggplot(
      data.frame(
        x = rnorm(10,0,1),
        y = rnorm(10,0,1)
      )
    ) +
    geom_point(aes(x = x, y = y))
  for(x in c("tiff", "pdf", "png")){
    file <- tempfile()

    options(
      figure_width = 7,
      figure_height = 4,
      figure_dpi = 700L,
      figure_device = x,
      figure_family = "Calibri Light"
    )

    rcsave(file = file, plot = plt)
    expect_true(file.exists(file))
  }

  options(op)

})
