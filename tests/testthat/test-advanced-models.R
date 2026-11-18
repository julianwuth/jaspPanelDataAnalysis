context("Hausman-Taylor, GMM, FGLS and variable coefficient models")

wagesFrame <- function() {
  return(plm::pdata.frame(wages(), index = c("id", "period")))
}

htOptions <- function(...) {
  return(panelOptions(
    "htModel", dependent = "lwage",
    timeVaryingExogenous    = c("wks", "isSouth", "isSmsa"),
    timeVaryingEndogenous   = c("exp", "isMarried", "isUnion"),
    timeInvariantExogenous  = c("female", "isBlack"),
    timeInvariantEndogenous = "ed",
    id = "id", time = "period", estimates = TRUE, ...
  ))
}

htFormula <- lwage ~ wks + isSouth + isSmsa + exp + isMarried + isUnion + female + isBlack + ed |
  wks + isSouth + isSmsa + female + isBlack | exp + isMarried + isUnion

test_that("Hausman-Taylor handles time-varying endogenous regressors", {
  results  <- runPanel("htModel", wages(), htOptions())
  expected <- plm::plm(htFormula, data = wagesFrame(), model = "random", random.method = "ht", inst.method = "baltagi")

  # by position: current jaspTools may type the 0/1 columns as nominal, which changes
  # their coefficient names but not their estimates
  expect_equal(panelColumn(results, "coefTable", "estimate"), unname(stats::coef(expected)), tolerance = 1e-8)

  # treating exp, married and union as exogenous would give about 0.45
  expect_equal(panelCell(results, "coefTable", "estimate", "ed"), 0.1242, tolerance = 1e-3)
})

test_that("the instrument sets differ", {
  amemiya <- runPanel("htModel", wages(), htOptions(htMethod = "am"))
  bms     <- runPanel("htModel", wages(), htOptions(htMethod = "bms"))

  expect_false(isTRUE(all.equal(panelColumn(amemiya, "coefTable", "estimate"), panelColumn(bms, "coefTable", "estimate"))))
})

test_that("Hausman-Taylor ignores a stored robust option", {
  classical <- runPanel("htModel", wages(), htOptions())
  robust    <- runPanel("htModel", wages(), htOptions(robustEstimates = TRUE))

  expect_equal(panelColumn(robust, "coefTable", "se"), panelColumn(classical, "coefTable", "se"))
})

test_that("Hausman-Taylor checks the classification of the regressors", {
  options <- htOptions(timeVaryingExogenous = c("wks", "ed"), timeInvariantEndogenous = list())
  results <- runPanel("htModel", wages(), options)
  expect_match(results[["results"]][["errorMessage"]], "ed is listed as time-varying")

  options <- htOptions(timeInvariantExogenous = c("female", "wks"), timeVaryingExogenous = "isSouth",
                       timeInvariantEndogenous = list())
  results <- runPanel("htModel", wages(), options)
  expect_match(results[["results"]][["errorMessage"]], "wks is listed as time-invariant")

  options <- htOptions(timeVaryingExogenous = "wks", timeVaryingEndogenous = list(),
                       timeInvariantExogenous = list(), timeInvariantEndogenous = c("female", "ed"))
  results <- runPanel("htModel", wages(), options)
  expect_match(panelError(results, "modelSummaryTable"), "at least as many exogenous time-varying regressors")
})

test_that("Hausman-Taylor warns on unbalanced panels", {
  data <- wages()
  data <- data[-seq(1, nrow(data), by = 11), ]

  results <- runPanel("htModel", data, htOptions())
  expect_panelFootnote(results, "modelSummaryTable", "one quasi-demeaning parameter")
})

gmmOptions <- function(...) {
  return(panelOptions("gmmModel", dependent = "lwage", covariates = "wks", id = "id", time = "period", ...))
}

gmmFit <- function(...) {
  return(suppressWarnings(plm::pgmm(lwage ~ lag(lwage, 1) + wks | lag(lwage, 2:99), data = wagesFrame(), ...)))
}

test_that("one-step GMM reports robust standard errors", {
  results  <- runPanel("gmmModel", wages(), gmmOptions())
  expected <- plm::vcovHC(gmmFit(effect = "individual", model = "onestep", transformation = "d"))

  expect_equal(panelColumn(results, "coefTable", "se"), unname(sqrt(diag(expected))), tolerance = 1e-8)
  expect_equal(panelColumn(results, "coefTable", "se"), c(0.025153186, 0.002356702), tolerance = 1e-6)
  expect_panelFootnote(results, "coefTable", "robust to heteroskedasticity")

  # the checkbox only applies to two-step estimation
  unchecked <- runPanel("gmmModel", wages(), gmmOptions(robustSE = FALSE))
  expect_equal(panelColumn(unchecked, "coefTable", "se"), panelColumn(results, "coefTable", "se"))
})

test_that("two-step GMM offers Windmeijer and conventional standard errors", {
  fit <- gmmFit(effect = "individual", model = "twosteps", transformation = "d")

  corrected <- runPanel("gmmModel", wages(), gmmOptions(gmmSteps = "twosteps"))
  expect_equal(panelColumn(corrected, "coefTable", "se"), unname(sqrt(diag(plm::vcovHC(fit)))), tolerance = 1e-8)
  expect_panelFootnote(corrected, "coefTable", "Windmeijer")

  conventional <- runPanel("gmmModel", wages(), gmmOptions(gmmSteps = "twosteps", robustSE = FALSE))
  expect_equal(panelColumn(conventional, "coefTable", "se"), unname(sqrt(diag(stats::vcov(fit)))), tolerance = 1e-8)
  expect_panelFootnote(conventional, "coefTable", "far too small")
})

test_that("the overidentification test always uses the two-step estimator", {
  expected <- plm::sargan(gmmFit(effect = "individual", model = "twosteps", transformation = "d"), "twosteps")

  for (steps in c("onestep", "twosteps")) {
    results <- runPanel("gmmModel", wages(), gmmOptions(gmmSteps = steps))

    expect_equal(panelCell(results, "gmmTestsTable", "statistic", "Hansen J", "test"), unname(expected$statistic), tolerance = 1e-6)
    expect_equal(as.numeric(panelCell(results, "gmmTestsTable", "df", "Hansen J", "test")), 14)
  }
})

test_that("system GMM needs time effects", {
  results <- runPanel("gmmModel", wages(), gmmOptions(transformation = "ld"))
  expect_match(panelError(results, "modelSummaryTable"), "System GMM requires individual and time effects")

  results  <- runPanel("gmmModel", wages(), gmmOptions(transformation = "ld", effects = "twoways"))
  expected <- gmmFit(effect = "twoways", model = "onestep", transformation = "ld")
  expect_equal(panelColumn(results, "coefTable", "estimate"), unname(stats::coef(expected)[1:2]), tolerance = 1e-8)
  expect_panelFootnote(results, "modelSummaryTable", "uncorrelated with the individual effects")
})

test_that("endogenous covariates have their own smallest instrument lag", {
  options <- gmmOptions(covariates = list(), endogenousCovariates = "wks", endogenousLagMin = 1L)
  results <- runPanel("gmmModel", wages(), options)

  expected <- suppressWarnings(plm::pgmm(lwage ~ lag(lwage, 1) + wks | lag(lwage, 2:99) + lag(wks, 1:99),
                                         data = wagesFrame(), effect = "individual", model = "onestep",
                                         transformation = "d"))
  expect_equal(panelColumn(results, "coefTable", "estimate"), unname(stats::coef(expected)), tolerance = 1e-8)
})

shortPanel <- function(nPeriods) {
  set.seed(7)
  data <- expand.grid(unit = factor(1:60), wave = seq_len(nPeriods))
  data <- data[order(data$unit, data$wave), ]
  data$x1 <- rnorm(nrow(data))
  data$x2 <- rnorm(nrow(data))
  effect  <- rnorm(60)[data$unit]
  data$y  <- effect + 0.5 * data$x1 - 0.3 * data$x2 + rnorm(nrow(data))
  data$wave <- ordered(data$wave)

  return(data)
}

test_that("GMM keeps valid estimates on the shortest usable panel", {
  data    <- shortPanel(3)
  options <- panelOptions("gmmModel", dependent = "y", covariates = c("x1", "x2"), id = "unit", time = "wave")
  results <- runPanel("gmmModel", data, options)

  expected <- suppressWarnings(plm::pgmm(y ~ lag(y, 1) + x1 + x2 | lag(y, 2:99),
                                         data = plm::pdata.frame(data, c("unit", "wave")),
                                         effect = "individual", model = "onestep", transformation = "d"))

  expect_equal(panelColumn(results, "coefTable", "estimate"), unname(stats::coef(expected)), tolerance = 1e-8)
  expect_false("Arellano-Bond AR(2)" %in% panelColumn(results, "gmmTestsTable", "test"))
})

test_that("exactly identified GMM has no overidentification test", {
  options <- panelOptions("gmmModel", dependent = "y", covariates = "x1", id = "unit", time = "wave",
                          instrumentLagMax = 2L, collapseInstruments = TRUE)
  results <- runPanel("gmmModel", shortPanel(5), options)

  expect_false("Hansen J" %in% panelColumn(results, "gmmTestsTable", "test"))
  expect_panelFootnote(results, "gmmTestsTable", "exactly identified")
})

test_that("lag settings are checked against the panel length", {
  data    <- emplUK()
  options <- panelOptions("gmmModel", dependent = "emp", covariates = "wage", id = "firm", time = "year",
                          instrumentLagMin = 20L)
  results <- runPanel("gmmModel", data, options)

  expect_match(panelError(results, "modelSummaryTable"), "require at least 21 time periods")
})

producData <- function() {
  data       <- panelData("Produc")
  data$state <- factor(data$state)
  data$year  <- ordered(data$year)
  data$logGsp  <- log(data$gsp)
  data$logPcap <- log(data$pcap)
  data$logPc   <- log(data$pc)

  return(data)
}

pgglsProduc <- function(...) {
  return(panelOptions("pgglsModel", dependent = "logGsp", covariates = c("logPcap", "logPc", "emp"),
                      id = "state", time = "year", ...))
}

test_that("general FGLS needs enough groups for its covariance matrix", {
  options <- panelOptions("pgglsModel", dependent = "inv", covariates = c("value", "capital"), id = "firm", time = "year")

  # 10 firms cannot estimate a 19 by 19 matrix
  results <- runPanel("pgglsModel", grunfeld(), options)
  expect_match(panelError(results, "modelSummaryTable"), "19 by 19")
  expect_length(panelTable(results, "coefTable")[["data"]], 0)

  # with time effects 20 periods estimate a 9 by 9 matrix
  options$effects <- "time"
  results <- runPanel("pgglsModel", grunfeld(), options)
  expect_identical(panelTable(results, "modelSummaryTable")[["status"]], "complete")
  expect_length(panelColumn(results, "coefTable", "estimate"), 2)
})

test_that("the FGLS R-squared uses the transformed dependent variable", {
  data <- producData()

  for (estimator in c("within", "fd", "pooling")) {
    results  <- runPanel("pgglsModel", data, pgglsProduc(pgglsEstimator = estimator))
    fit      <- plm::pggls(logGsp ~ logPcap + logPc + emp, data = plm::pdata.frame(data, c("state", "year")),
                           model = estimator)
    response <- as.numeric(plm::pmodel.response(logGsp ~ 1, data = plm::pdata.frame(data, c("state", "year")),
                                                model = estimator))
    expected <- 1 - sum(as.numeric(stats::residuals(fit))^2) / sum((response - mean(response))^2)

    expect_equal(panelColumn(results, "modelSummaryTable", "rSq"), expected, tolerance = 1e-8, info = estimator)
    expect_lt(panelColumn(results, "modelSummaryTable", "rSq"), 1)
  }
})

test_that("FGLS on first differences rejects gaps", {
  data    <- producData()
  data    <- data[-5, ]
  results <- runPanel("pgglsModel", data, pgglsProduc(pgglsEstimator = "fd"))

  expect_match(panelError(results, "modelSummaryTable"), "consecutive time periods")
})

vcmOptions <- function(...) {
  return(panelOptions("vcmModel", dependent = "inv", covariates = c("value", "capital"), id = "firm", time = "year", ...))
}

test_that("the homogeneity test counts the groups of the chosen dimension", {
  individual <- runPanel("vcmModel", grunfeld(), vcmOptions(vcmEstimator = "random"))
  expect_equal(panelColumn(individual, "vcmHomogeneityTable", "df"), 27)

  periods   <- runPanel("vcmModel", grunfeld(), vcmOptions(vcmEstimator = "random", effects = "time"))
  statistic <- panelColumn(periods, "vcmHomogeneityTable", "chiSq")

  expect_equal(panelColumn(periods, "vcmHomogeneityTable", "df"), 57)
  expect_equal(panelColumn(periods, "vcmHomogeneityTable", "pVal"),
               stats::pchisq(statistic, 57, lower.tail = FALSE))
  expect_gt(panelColumn(periods, "vcmHomogeneityTable", "pVal"), 0.05)
})

test_that("the variable coefficients R-squared refers to the dependent variable", {
  data  <- grunfeld()
  frame <- plm::pdata.frame(data, c("firm", "year"))

  for (estimator in c("within", "random")) {
    for (effect in c("individual", "time")) {
      fit      <- plm::pvcm(inv ~ value + capital, data = frame, model = estimator, effect = effect)
      expected <- 1 - sum(as.numeric(stats::residuals(fit))^2) / sum((data$inv - mean(data$inv))^2)
      results  <- runPanel("vcmModel", data, vcmOptions(vcmEstimator = estimator, effects = effect))

      expect_equal(panelColumn(results, "modelSummaryTable", "rSq"), expected, tolerance = 1e-8)
    }
  }
})

test_that("the uncorrected coefficient covariance matrix is disclosed", {
  results <- runPanel("vcmModel", grunfeld(), vcmOptions(vcmEstimator = "random"))
  expect_panelFootnote(results, "vcmVarianceTable", "not positive semidefinite")
})
