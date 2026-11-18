context("Static panel models")

# Expected values are derived independently of the module: from base R regressions,
# or from plm where the module must reproduce it.

staticOptions <- function(analysis, ...) {
  return(panelOptions(analysis, dependent = "inv", covariates = c("value", "capital"),
                      id = "firm", time = "year", estimates = TRUE, ...))
}

panelFrame <- function(data) {
  return(plm::pdata.frame(data, index = c("firm", "year")))
}

clusterVcov <- function(regressors, residuals, cluster) {
  bread  <- solve(crossprod(regressors))
  scores <- rowsum(regressors * residuals, as.character(cluster))

  return(bread %*% crossprod(scores) %*% bread)
}

# one-period differences only, computed by hand
oneperiodDifferences <- function(data) {
  data <- data[stats::complete.cases(data[c("inv", "value", "capital")]), ]
  data$yearNumber <- as.numeric(as.character(data$year))
  data <- data[order(data$firm, data$yearNumber), ]

  rows <- which(data$firm[-1] == data$firm[-nrow(data)] & diff(data$yearNumber) == 1) + 1

  return(data.frame(
    firm    = data$firm[rows],
    inv     = data$inv[rows] - data$inv[rows - 1],
    value   = data$value[rows] - data$value[rows - 1],
    capital = data$capital[rows] - data$capital[rows - 1]
  ))
}

test_that("first differences do not span gaps or removed rows", {
  data <- grunfeld()
  year <- as.numeric(as.character(data$year))
  data <- data[!(data$firm == 1 & year %in% 1940:1942) & !(data$firm == 2 & year == 1950), ]

  differences <- oneperiodDifferences(data)
  expected    <- stats::lm(inv ~ value + capital, data = differences)

  results <- runPanel("firstDifferenceModel", data, staticOptions("firstDifferenceModel"))

  expect_equal(panelCell(results, "coefTable", "estimate", "value"), stats::coef(expected)[["value"]], tolerance = 1e-8)
  expect_equal(panelCell(results, "coefTable", "estimate", "capital"), stats::coef(expected)[["capital"]], tolerance = 1e-8)
  expect_equal(panelCell(results, "coefTable", "se", "value"),
               summary(expected)$coefficients["value", "Std. Error"], tolerance = 1e-8)
  expect_equal(panelColumn(results, "modelSummaryTable", "df2"), stats::df.residual(expected))
  expect_panelFootnote(results, "modelSummaryTable", "more than one period were not used: 2")

  # a missing value breaks the sequence in the same way
  data <- grunfeld()
  data$value[data$firm == 3 & as.numeric(as.character(data$year)) %in% 1945:1947] <- NA
  expected <- stats::lm(inv ~ value + capital, data = oneperiodDifferences(data))
  results  <- runPanel("firstDifferenceModel", data, staticOptions("firstDifferenceModel"))
  expect_equal(panelCell(results, "coefTable", "estimate", "capital"), stats::coef(expected)[["capital"]], tolerance = 1e-8)
})

test_that("robust first differences cluster on the individual across gaps", {
  data <- grunfeld()
  year <- as.numeric(as.character(data$year))
  data <- data[!(data$firm == 1 & year %in% 1940:1942), ]

  differences <- oneperiodDifferences(data)
  expected    <- stats::lm(inv ~ value + capital, data = differences)
  vcovRobust  <- clusterVcov(stats::model.matrix(expected), stats::residuals(expected), differences$firm)

  results <- runPanel("firstDifferenceModel", data, staticOptions("firstDifferenceModel", robustEstimates = TRUE))

  expect_equal(panelCell(results, "coefTable", "se", "value"), sqrt(vcovRobust["value", "value"]), tolerance = 1e-8)
  expect_equal(panelColumn(results, "modelSummaryTable", "df2"), 9)
})

test_that("evenly spaced numeric waves count as consecutive", {
  data      <- grunfeld()
  data$year <- ordered(5 * as.integer(as.character(data$year)))

  regular <- runPanel("firstDifferenceModel", data, staticOptions("firstDifferenceModel"))
  annual  <- runPanel("firstDifferenceModel", grunfeld(), staticOptions("firstDifferenceModel"))

  expect_equal(panelColumn(regular, "coefTable", "estimate"), panelColumn(annual, "coefTable", "estimate"))
  expect_false(any(grepl("more than one period", panelFootnotes(regular, "modelSummaryTable"))))
})

test_that("two-way random effects on an unbalanced panel have the GLS covariance matrix", {
  data <- grunfeldUnbalanced()
  fit  <- plm::plm(inv ~ value + capital, data = panelFrame(data), model = "random", effect = "twoways")

  sigma2 <- plm::ercomp(fit)$sigma2
  frame  <- as.data.frame(panelFrame(data))
  ids    <- stats::model.matrix(~ 0 + factor(firm), frame)
  years  <- stats::model.matrix(~ 0 + factor(year), frame)
  omega  <- sigma2[["idios"]] * diag(nrow(frame)) + sigma2[["id"]] * tcrossprod(ids) + sigma2[["time"]] * tcrossprod(years)
  design <- cbind("(Intercept)" = 1, value = frame$value, capital = frame$capital)
  vcovGLS <- solve(t(design) %*% solve(omega, design))

  results <- runPanel("randomModel", data, staticOptions("randomModel", effects = "twoways"))

  expect_equal(panelColumn(results, "coefTable", "se"), unname(sqrt(diag(vcovGLS))), tolerance = 1e-6)

  terms <- c("value", "capital")
  wald  <- drop(crossprod(stats::coef(fit)[terms], solve(vcovGLS[terms, terms], stats::coef(fit)[terms])))
  expect_equal(panelColumn(results, "modelSummaryTable", "statistic"), wald, tolerance = 1e-6)
  expect_identical(panelColumn(results, "modelSummaryTable", "rSq"), "")
  expect_panelFootnote(results, "modelSummaryTable", "R-squared is not reported")

  robust <- runPanel("randomModel", data, staticOptions("randomModel", effects = "twoways", robustEstimates = TRUE))
  expect_match(panelError(robust, "coefTable"), "not available")
})

test_that("two-way fixed effects match the dummy variable regression on unbalanced panels", {
  data     <- grunfeldUnbalanced()
  data$firmFactor <- factor(data$firm)
  data$yearFactor <- factor(as.character(data$year))
  lsdv     <- stats::lm(inv ~ value + capital + firmFactor + yearFactor, data = data)
  estimate <- stats::coef(lsdv)

  results <- runPanel("fixedModel", data, staticOptions("fixedModel", effects = "twoways", fixedEffects = TRUE,
                                                        fixedEffectsType = "dfirst"))
  rows <- panelTable(results, "fixedEffTable")[["data"]]
  get  <- function(type, label) rows[[which(vapply(rows, function(r) r$type == type && r$effect == label, TRUE))]]$estimate

  expect_equal(get("Individual", "2"), estimate[["firmFactor2"]], tolerance = 1e-6)
  expect_equal(get("Individual", "10"), estimate[["firmFactor10"]], tolerance = 1e-6)

  firstYear <- levels(data$yearFactor)[1]
  expect_identical(firstYear, "1935")
  expect_equal(get("Time", "1941"), estimate[["yearFactor1941"]], tolerance = 1e-6)
  expect_equal(get("Time", "1954"), estimate[["yearFactor1954"]], tolerance = 1e-6)
})

test_that("two-way fixed effects equal plm on balanced panels", {
  data <- grunfeld()
  fit  <- plm::plm(inv ~ value + capital, data = panelFrame(data), model = "within", effect = "twoways")

  for (type in c("level", "dfirst", "dmean")) {
    results <- runPanel("fixedModel", data, staticOptions("fixedModel", effects = "twoways", fixedEffects = TRUE,
                                                          fixedEffectsType = type))
    rows <- panelTable(results, "fixedEffTable")[["data"]]
    expected <- c(plm::fixef(fit, effect = "individual", type = type), plm::fixef(fit, effect = "time", type = type))
    expect_equal(vapply(rows, `[[`, 0, "estimate"), as.numeric(expected), tolerance = 1e-6, info = type)
  }
})

test_that("demeaned fixed effects have the standard error of the demeaned estimate", {
  for (data in list(grunfeld(), grunfeldUnbalanced())) {
    lsdv     <- stats::lm(inv ~ 0 + firm + value + capital, data = data)
    counts   <- as.numeric(table(data$firm))
    weights  <- counts / sum(counts)
    terms    <- names(stats::coef(lsdv))
    contrast <- matrix(0, nrow = 10, ncol = length(terms), dimnames = list(NULL, terms))
    contrast[, paste0("firm", levels(data$firm))] <- diag(10) - matrix(weights, 10, 10, byrow = TRUE)
    expected <- sqrt(diag(contrast %*% stats::vcov(lsdv) %*% t(contrast)))

    results <- runPanel("fixedModel", data, staticOptions("fixedModel", fixedEffects = TRUE, fixedEffectsType = "dmean"))

    expect_equal(panelColumn(results, "fixedEffTable", "se"), unname(expected), tolerance = 1e-8)
    expect_equal(panelColumn(results, "fixedEffTable", "testStatistic"),
                 panelColumn(results, "fixedEffTable", "estimate") / unname(expected), tolerance = 1e-8)
  }
})

test_that("the fixed effects adjusted R-squared uses the within degrees of freedom", {
  results <- runPanel("fixedModel", grunfeld(), staticOptions("fixedModel"))
  rSq     <- panelColumn(results, "modelSummaryTable", "rSq")

  # n - N = 190 total and n - N - K = 188 residual degrees of freedom
  expect_equal(panelColumn(results, "modelSummaryTable", "adjRSq"), 1 - (1 - rSq) * 190 / 188)
})

test_that("robust standard errors also drive the overall test", {
  data <- grunfeld()
  fit  <- plm::plm(inv ~ value + capital, data = panelFrame(data), model = "within")

  demeaned <- stats::model.matrix(fit)
  vcovRobust <- clusterVcov(demeaned, as.numeric(stats::residuals(fit)), plm::index(fit)[[1]])
  beta <- stats::coef(fit)
  expectedF <- drop(crossprod(beta, solve(vcovRobust, beta))) / 2

  results <- runPanel("fixedModel", data, staticOptions("fixedModel", robustEstimates = TRUE))

  expect_equal(panelColumn(results, "modelSummaryTable", "statistic"), expectedF, tolerance = 1e-6)
  expect_equal(panelColumn(results, "modelSummaryTable", "df1"), 2)
  expect_equal(panelColumn(results, "modelSummaryTable", "df2"), 9)
  expect_equal(panelColumn(results, "modelSummaryTable", "pVal"),
               stats::pf(expectedF, 2, 9, lower.tail = FALSE), tolerance = 1e-6)
  expect_panelFootnote(results, "modelSummaryTable", "cluster-robust")

  statistic <- beta[["value"]] / sqrt(vcovRobust["value", "value"])
  expect_equal(panelCell(results, "coefTable", "pVal", "value"),
               2 * stats::pt(-abs(statistic), df = 9), tolerance = 1e-6)
  expect_panelFootnote(results, "coefTable", "many individuals relative")

  # random effects use a chi-squared Wald test
  fit <- plm::plm(inv ~ value + capital, data = panelFrame(data), model = "random")
  expected <- plm::pwaldtest(fit, test = "Chisq", vcov = function(x) plm::vcovHC(x, method = "arellano", type = "HC0"))
  results <- runPanel("randomModel", data, staticOptions("randomModel", robustEstimates = TRUE))
  expect_equal(panelColumn(results, "modelSummaryTable", "statistic"), unname(expected$statistic), tolerance = 1e-6)
})

test_that("the Hausman test uses one error variance and excludes aggregate time variables", {
  data    <- grunfeld()
  aux     <- plm::phtest(inv ~ value + capital, data = panelFrame(data), method = "aux")
  results <- runPanel("randomModel", data, staticOptions("randomModel", hausmanTest = TRUE))

  # with one error variance the statistic equals the regression form
  # (Wooldridge, 2002, p. 290, Eq. 10.79)
  expect_equal(panelColumn(results, "hausmanTestTable", "chiSq"), unname(aux$statistic), tolerance = 1e-6)
  expect_equal(panelColumn(results, "hausmanTestTable", "df"), 2)

  # every variance component estimator must give a positive statistic
  for (estimator in c("walhus", "amemiya", "nerlove")) {
    results <- runPanel("randomModel", data, staticOptions("randomModel", hausmanTest = TRUE, estimators = estimator))
    expect_gt(panelColumn(results, "hausmanTestTable", "chiSq"), 0)
  }

  data$period <- factor(as.character(data$year))
  options <- panelOptions("fixedModel", dependent = "inv", covariates = c("value", "capital"), factors = "period",
                          id = "firm", time = "year", hausmanTest = TRUE)
  results <- runPanel("fixedModel", data, options)
  expect_equal(panelColumn(results, "hausmanTestTable", "df"), 2)
})

test_that("the robust Hausman test matches the auxiliary regression", {
  data     <- grunfeld()
  expected <- plm::phtest(inv ~ value + capital, data = panelFrame(data), method = "aux",
                          vcov = function(x) plm::vcovHC(x, method = "arellano", type = "HC0"))
  results  <- runPanel("randomModel", data, staticOptions("randomModel", hausmanTest = TRUE, robustEstimates = TRUE))

  expect_equal(panelColumn(results, "hausmanTestTable", "chiSq"), unname(expected$statistic), tolerance = 1e-6)
  expect_panelFootnote(results, "hausmanTestTable", "Cluster-robust")
})

test_that("Lagrange multiplier output names the test and its assumptions", {
  options <- panelOptions("poolingModel", dependent = "inv", covariates = c("value", "capital"), id = "firm",
                          time = "year", lmTest = TRUE)
  results <- runPanel("poolingModel", grunfeld(), options)
  fit     <- plm::plm(inv ~ value + capital, data = panelFrame(grunfeld()), model = "pooling")

  expect_identical(panelColumn(results, "lmTestTable", "test"), "Honda")
  expect_equal(panelColumn(results, "lmTestTable", "statistic"),
               unname(plm::plmtest(fit, effect = "individual", type = "honda")$statistic))
  expect_panelFootnote(results, "lmTestTable", "one-sided")
  expect_panelFootnote(results, "lmTestTable", "does not establish a random effects structure")

  options$lmTestType <- "bp"
  results <- runPanel("poolingModel", grunfeld(), options)
  expect_identical(panelColumn(results, "lmTestTable", "test"), "Breusch-Pagan")
  expect_panelFootnote(results, "lmTestTable", "chi-squared")
})

test_that("a zero variance component is disclosed", {
  set.seed(42)
  data <- expand.grid(firm = factor(1:40), year = ordered(1:5))
  data <- data[order(data$firm, data$year), ]
  data$x <- rnorm(nrow(data))
  # errors that sum to zero within each individual imply a negative effect variance
  error <- rnorm(nrow(data))
  data$y <- 1 + 0.5 * data$x + error - stats::ave(error, data$firm)

  options <- panelOptions("randomModel", dependent = "y", covariates = "x", id = "firm", time = "year",
                          estimates = TRUE, randomEffects = TRUE)
  results <- runPanel("randomModel", data, options)
  pooled  <- stats::lm(y ~ x, data = data)

  expect_equal(panelCell(results, "coefTable", "estimate", "x"), stats::coef(pooled)[["x"]], tolerance = 1e-8)
  expect_panelFootnote(results, "coefTable", "individual effect was zero or negative")
  expect_panelFootnote(results, "randEffTable", "individual effect was zero or negative")
})

test_that("regressors removed by the transformation are named", {
  data          <- grunfeld()
  data$sector   <- as.numeric(data$firm) * 2.5
  options       <- panelOptions("fixedModel", dependent = "inv", covariates = c("value", "sector"), id = "firm",
                                time = "year", estimates = TRUE)
  results <- runPanel("fixedModel", data, options)
  expect_panelFootnote(results, "coefTable", "constant after the transformation or collinear: sector")

  options <- panelOptions("poolingModel", dependent = "inv", covariates = c("value", "sector"), id = "firm",
                          time = "year", estimates = TRUE)
  results <- runPanel("poolingModel", data, options)
  expect_false(any(grepl("constant after the transformation", panelFootnotes(results, "coefTable"))))

  results <- runPanel("firstDifferenceModel", grunfeld(), staticOptions("firstDifferenceModel"))
  expect_panelFootnote(results, "coefTable", "linear time trend")
})

test_that("disconnected two-way panels are reported", {
  data <- grunfeld()
  year <- as.numeric(as.character(data$year))
  data <- data[(as.numeric(data$firm) <= 5 & year < 1945) | (as.numeric(data$firm) > 5 & year >= 1945), ]

  results <- runPanel("fixedModel", data, staticOptions("fixedModel", effects = "twoways", fixedEffects = TRUE))
  expect_panelFootnote(results, "modelSummaryTable", "connected panel")
  expect_match(panelError(results, "fixedEffTable"), "connected panel")
})
