context("Panel line plot")

plotOptions <- function(...) {
  return(panelOptions("poolingModel", dependent = "inv", covariates = "value", id = "firm", time = "year",
                      plot = TRUE, ...))
}

panelPlot <- function(results) {
  plotName <- results[["results"]][["plmPlot"]][["data"]]

  return(results[["state"]][["figures"]][[plotName]][["obj"]])
}

axisLabels <- function(plot) {
  return(ggplot2::ggplot_build(plot)$layout$panel_params[[1]]$x$get_labels())
}

test_that("text time labels stay on the axis", {
  data      <- grunfeld()
  quarters  <- paste0(rep(1935:1939, each = 4), "Q", 1:4)
  data$year <- ordered(quarters[as.integer(data$year)], levels = quarters)

  labels <- axisLabels(panelPlot(runPanel("poolingModel", data, plotOptions())))

  expect_true(all(labels %in% quarters))
  expect_true(all(c("1935Q1", "1939Q4") %in% labels))
})

test_that("numeric time axes have whole, distinct ticks", {
  data <- droplevels(grunfeld()[as.numeric(as.character(grunfeld()$year)) <= 1936, ])

  labels <- axisLabels(panelPlot(runPanel("poolingModel", data, plotOptions())))

  expect_identical(as.character(labels), c("1935", "1936"))
})

test_that("the legend is dropped for many individuals", {
  wide <- panelPlot(runPanel("poolingModel", grunfeld(), plotOptions()))
  expect_identical(wide$theme$legend.position, "right")

  data <- wages()
  options <- panelOptions("poolingModel", dependent = "lwage", covariates = "wks", id = "id", time = "period", plot = TRUE)
  many <- panelPlot(runPanel("poolingModel", data, options))
  expect_identical(many$theme$legend.position, "none")
})

test_that("lines are not drawn across missing periods", {
  data <- grunfeld()
  year <- as.numeric(as.character(data$year))
  data$inv[data$firm == 2 & year == 1950] <- NA
  data <- data[!(data$firm == 1 & year %in% 1940:1942), ]

  built <- ggplot2::ggplot_build(panelPlot(runPanel("poolingModel", data, plotOptions())))$data[[1]]

  # ten firms, two of which are split in two
  expect_length(unique(built$group), 12)
})

test_that("isolated observations remain visible", {
  data <- grunfeld()
  year <- as.numeric(as.character(data$year))
  data <- data[!(data$firm == 1 & year %in% c(1940, 1942)), ]

  built <- ggplot2::ggplot_build(panelPlot(runPanel("poolingModel", data, plotOptions())))$data[[2]]

  expect_equal(built$x, 1941)
})
