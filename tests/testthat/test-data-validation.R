context("Panel data validation")

grunfeldOptions <- function(analysis, ...) {
  return(panelOptions(analysis, dependent = "inv", covariates = c("value", "capital"),
                      id = "firm", time = "year", ...))
}

allAnalyses <- c("fixedModel", "randomModel", "poolingModel", "firstDifferenceModel", "betweenModel",
                 "vcmModel", "pgglsModel", "gmmModel")

test_that("rows without an id or period are dropped instead of crashing the estimators", {
  data <- grunfeld()
  data$firm[3] <- NA

  # lag() segfaulted on a missing id, which try() cannot catch
  results <- runPanel("gmmModel", data, grunfeldOptions("gmmModel", robustSE = TRUE))
  expected <- suppressWarnings(plm::pgmm(
    inv ~ lag(inv, 1) + value + capital | lag(inv, 2:99),
    data = plm::pdata.frame(grunfeld()[-3, ], index = c("firm", "year")),
    effect = "individual", model = "onestep", transformation = "d"
  ))
  expect_equal(unname(panelColumn(results, "coefTable", "estimate")),
               unname(stats::coef(expected)), tolerance = 1e-8)

  results  <- runPanel("fixedModel", data, grunfeldOptions("fixedModel", estimates = TRUE))
  expected <- plm::plm(inv ~ value + capital, data = plm::pdata.frame(grunfeld()[-3, ], c("firm", "year")))
  expect_equal(panelCell(results, "coefTable", "estimate", "value"),
               stats::coef(expected)[["value"]], tolerance = 1e-8)
  expect_panelFootnote(results, "modelSummaryTable", "Rows excluded because of missing values: 1")
})

test_that("duplicate id-time pairs stop every analysis", {
  data <- rbind(grunfeld(), grunfeld()[1, ])

  for (analysis in allAnalyses) {
    results <- runPanel(analysis, data, grunfeldOptions(analysis))
    expect_identical(results[["status"]], "validationError", info = analysis)
    expect_match(results[["results"]][["errorMessage"]], "must occur only once", info = analysis)
  }
})

test_that("order-dependent models need an ordered time variable", {
  # current jaspTools converts the column to the type the form allows, so the check
  # is exercised directly
  hasTimeOrder <- jaspPanelDataAnalysis:::.hasTimeOrderPD

  expect_false(hasTimeOrder(factor(c("spring", "summer"))))
  expect_true(hasTimeOrder(ordered(c("spring", "summer"))))
  expect_true(hasTimeOrder(factor(c("2001", "2002"))))
  expect_true(hasTimeOrder(c(2001, 2002)))

  timeOrderMatters <- jaspPanelDataAnalysis:::.timeOrderMattersPD

  expect_true(timeOrderMatters(list(analysis = "fd")))
  expect_true(timeOrderMatters(list(analysis = "gmm")))
  expect_true(timeOrderMatters(list(analysis = "pggls", pgglsEstimator = "fd")))
  expect_false(timeOrderMatters(list(analysis = "pggls", pgglsEstimator = "within")))
  expect_false(timeOrderMatters(list(analysis = "within")))
})

test_that("numeric time labels are ordered numerically", {
  ordinal <- runPanel("firstDifferenceModel", grunfeld(), grunfeldOptions("firstDifferenceModel", estimates = TRUE))

  data      <- grunfeld()
  data$year <- factor(as.character(as.integer(as.character(data$year)) - 1930L))  # 5, 6, ..., 24 sort as text
  results   <- runPanel("firstDifferenceModel", data, grunfeldOptions("firstDifferenceModel", estimates = TRUE))

  expect_equal(panelColumn(results, "coefTable", "estimate"),
               panelColumn(ordinal, "coefTable", "estimate"), tolerance = 1e-10)
})

test_that("text labels sorted out of order raise a warning", {
  data      <- grunfeld()
  data$year <- ordered(paste0("t", as.integer(data$year)))  # levels t1, t10, t11, ...

  results <- runPanel("firstDifferenceModel", data, grunfeldOptions("firstDifferenceModel"))
  expect_panelFootnote(results, "modelSummaryTable", "chronological order")

  looksUnsorted <- jaspPanelDataAnalysis:::.timeLabelsLookUnsortedPD

  expect_true(looksUnsorted(ordered(c("t1", "t2", "t10"), levels = c("t1", "t10", "t2"))))
  expect_false(looksUnsorted(ordered(c("t1", "t2", "t10"), levels = c("t1", "t2", "t10"))))
  expect_false(looksUnsorted(ordered(c("spring", "summer"))))
})

test_that("ordered factors use treatment contrasts", {
  data      <- grunfeld()
  data$size <- cut(data$capital, 3, labels = c("S", "M", "L"))
  options   <- panelOptions("poolingModel", dependent = "inv", covariates = "value", factors = "size",
                            id = "firm", time = "year", estimates = TRUE)
  nominal   <- runPanel("poolingModel", data, options)

  data$size <- ordered(data$size)
  ordinal   <- runPanel("poolingModel", data, options)

  expect_equal(panelColumn(ordinal, "coefTable", "coef"), panelColumn(nominal, "coefTable", "coef"))
  expect_equal(panelColumn(ordinal, "coefTable", "estimate"), panelColumn(nominal, "coefTable", "estimate"))
  expect_true(all(c("sizeM", "sizeL") %in% panelColumn(ordinal, "coefTable", "coef")))
})

test_that("models without residual degrees of freedom report an error", {
  data <- droplevels(grunfeld()[grunfeld()$firm %in% 1:3, ])

  results <- runPanel("betweenModel", data, grunfeldOptions("betweenModel", estimates = TRUE))
  expect_match(panelError(results, "modelSummaryTable"), "too few observations")
  expect_length(panelTable(results, "coefTable")[["data"]], 0)
})

test_that("a constant dependent variable is reported", {
  data     <- grunfeld()
  data$inv <- 1

  results <- runPanel("fixedModel", data, grunfeldOptions("fixedModel"))
  expect_identical(results[["status"]], "validationError")
})

test_that("the panel used is described and internal errors are explained", {
  data <- grunfeld()
  data$inv[1:5] <- NA

  results <- runPanel("poolingModel", data, grunfeldOptions("poolingModel"))
  expect_panelFootnote(results, "modelSummaryTable", "195 observations on 10 individuals over 20 time periods")
  expect_panelFootnote(results, "modelSummaryTable", "missing values: 5")

  data         <- grunfeld()
  data$sector  <- as.numeric(data$firm)
  results <- runPanel("fixedModel", data, panelOptions("fixedModel", dependent = "inv", covariates = "sector",
                                                       id = "firm", time = "year"))
  expect_match(panelError(results, "modelSummaryTable"), "No regressors remain")
})

test_that("ID only with time effects carries a warning", {
  options <- panelOptions("fixedModel", dependent = "inv", covariates = c("value", "capital"), id = "firm",
                          idOnly = TRUE, effects = "twoways")
  results <- runPanel("fixedModel", grunfeld(), options)
  expect_panelFootnote(results, "modelSummaryTable", "numbered within each individual")
})
