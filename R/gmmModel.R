#' @import jaspBase
#' @export
gmmModel <- function(jaspResults, dataset, options, analysis = "gmm") {
  options <- .rewriteOptionsPD(options, analysis)
  ready   <- .isReadyPD(options)

  if (ready)
    .checkErrorsPD(dataset, options)

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready, deps = "robustSE")

  .coefficientsTablePD(jaspResults, dataset, options, ready, deps = "robustSE")

  .gmmSpecificationTestsTablePD(jaspResults, dataset, options, ready)

  if (options[["plot"]])
    .createPlmPlot(jaspResults, dataset, options, ready)

  return()
}

.gmmRegressorsPD <- function(options) {
  terms <- c(
    sprintf("lag(%1$s, 1:%2$s)", options[["dependent"]], options[["lagsDependent"]]),
    .regressorsPD(options)
  )

  return(paste(terms, collapse = " + "))
}

# In the differenced equation the dependent variable is a valid instrument from lag 2
# (Wooldridge, 2002, p. 304). A sequentially exogenous covariate is valid from lag 1
# (Wooldridge, 2002, p. 303, Eq. 11.12), hence the separate setting.
.gmmInstrumentsPD <- function(options) {
  dependent <- sprintf("lag(%1$s, %2$s:%3$s)", options[["dependent"]],
                       options[["instrumentLagMin"]], options[["instrumentLagMax"]])

  endogenous <- .variablesPD(options[["endogenousCovariates"]])

  if (length(endogenous) > 0)
    endogenous <- sprintf("lag(%1$s, %2$s:%3$s)", endogenous,
                          options[["endogenousLagMin"]], options[["instrumentLagMax"]])

  return(paste(c(dependent, endogenous), collapse = " + "))
}

.fitGmmPD <- function(formula, plmDf, options) {
  systemGmm <- options[["transformation"]] == "ld"

  # pgmm only adds a constant to the level equation together with the time dummies
  if (systemGmm && options[["effects"]] != "twoways")
    stop(gettext("System GMM requires individual and time effects: without them the level equation has no intercept."), call. = FALSE)

  nPeriods <- plm::pdim(plmDf)[["nT"]][["T"]]
  required <- max(options[["lagsDependent"]] + 1, options[["instrumentLagMin"]]) + (if (systemGmm) 2 else 1)

  if (nPeriods < required)
    stop(gettextf("These lag settings require at least %1$s time periods, but the panel has %2$s.", required, nPeriods),
         call. = FALSE)

  fit <- plm::pgmm(formula, data = plmDf,
                   effect         = options[["effects"]],
                   model          = options[["gmmSteps"]],
                   transformation = options[["transformation"]],
                   collapse       = options[["collapseInstruments"]])

  return(fit)
}

# plm stores the one-step covariance matrix without the error variance, so only its
# robust form is usable; the two-step robust form has the Windmeijer correction
.gmmRobustPD <- function(options) {
  return(options[["gmmSteps"]] == "onestep" || options[["robustSE"]])
}

# summary.pgmm() fails as a whole when an AR test cannot be computed, which happens on
# short panels where the estimates themselves are fine
.gmmSummaryPD <- function(fit, robust) {
  covariance   <- if (robust) plm::vcovHC(fit) else stats::vcov(fit)
  testCovariance <- if (robust) covariance else NULL
  estimate     <- stats::coef(fit)
  standardError <- sqrt(diag(covariance))
  statistic    <- estimate / standardError
  nTimeTerms   <- if (fit[["args"]][["effect"]] == "twoways") length(fit[["args"]][["namest"]]) else 0
  shown        <- seq_len(length(estimate) - nTimeTerms)
  optional     <- function(expression) tryCatch(expression, error = function(e) NULL)

  coefficients <- cbind(estimate, standardError, statistic, 2 * stats::pnorm(-abs(statistic)))

  return(list(
    coefficients = coefficients[shown, , drop = FALSE],
    m1           = optional(plm::mtest(fit, order = 1L, vcov = testCovariance)),
    m2           = optional(plm::mtest(fit, order = 2L, vcov = testCovariance)),
    wald.coef    = optional(plm::pwaldtest(fit, param = "coef", vcov = testCovariance))
  ))
}

.gmmStandardErrorFootnotePD <- function(options) {
  if (options[["gmmSteps"]] == "onestep")
    return(gettext("Standard errors are robust to heteroskedasticity and serial correlation within individuals."))

  if (options[["robustSE"]])
    return(gettext("Standard errors are robust and include the Windmeijer finite-sample correction."))

  return(gettext("Conventional two-step standard errors, which tend to be far too small in finite samples."))
}

# The overidentification statistic has its chi-squared limit only at the estimator
# with the optimal weighting matrix (Wooldridge, 2002, p. 201, Eq. 8.49; p. 424,
# Eq. 14.19), which is the two-step estimator even when one-step results are shown.
.gmmTwoStepFitPD <- function(jaspResults, dataset, options) {
  if (options[["gmmSteps"]] == "twosteps")
    return(jaspResults[["modelFit"]]$object)

  if (!is.null(jaspResults[["gmmTwoStepFit"]]))
    return(jaspResults[["gmmTwoStepFit"]]$object)

  twoStepFit <- createJaspState()
  twoStepFit$dependOn(.pdModelDeps)
  jaspResults[["gmmTwoStepFit"]] <- twoStepFit

  twoStepOptions                <- options
  twoStepOptions[["gmmSteps"]]  <- "twosteps"
  plmDf                         <- .pdataFramePD(jaspResults, dataset, options, TRUE)

  twoStepFit$object <- try(.callEstimatorPD(.createFormulaPD(options), plmDf, twoStepOptions), silent = TRUE)

  return(twoStepFit$object)
}

.gmmSpecificationTestsTablePD <- function(jaspResults, dataset, options, ready) {
  if (!is.null(jaspResults[["gmmTestsTable"]]))
    return()

  gmmTestsTable <- createJaspTable(title = gettext("Specification Tests"))
  gmmTestsTable$position <- 3
  gmmTestsTable$dependOn(c(.pdModelDeps, "robustSE"))
  gmmTestsTable$addCitation(.pdCitation)
  gmmTestsTable$addCitation("Arellano, M., & Bond, S. (1991). Some tests of specification for panel data: Monte Carlo evidence and an application to employment equations. The Review of Economic Studies, 58(2), 277-297.")

  gmmTestsTable$addColumnInfo(name = "test",      title = gettext("Test"),      type = "string")
  gmmTestsTable$addColumnInfo(name = "statistic", title = gettext("Statistic"), type = "number")
  gmmTestsTable$addColumnInfo(name = "df",        title = "df",                 type = "integer")
  gmmTestsTable$addColumnInfo(name = "pVal",      title = "p",                  type = "pvalue")
  gmmTestsTable$showSpecifiedColumnsOnly <- TRUE

  jaspResults[["gmmTestsTable"]] <- gmmTestsTable

  if (!ready)
    return()

  fit <- jaspResults[["modelFit"]]$object

  if (isTryError(fit))
    return()

  modelSummary <- try(.gmmSummaryPD(fit, .gmmRobustPD(options)), silent = TRUE)

  if (isTryError(modelSummary)) {
    gmmTestsTable$setError(gettextf("The specification tests could not be performed: %s", .cleanErrorPD(modelSummary)))
    return()
  }

  twoStepFit <- .gmmTwoStepFitPD(jaspResults, dataset, options)
  hansenTest <- if (isTryError(twoStepFit)) NULL else tryCatch(plm::sargan(twoStepFit, "twosteps"), error = function(e) NULL)

  # with as many instruments as coefficients there is nothing left to test
  exactlyIdentified <- !is.null(hansenTest) && unname(hansenTest[["parameter"]]) == 0

  if (exactlyIdentified)
    hansenTest <- NULL

  tests <- list(
    list(label = gettext("Hansen J"),            test = hansenTest),
    list(label = gettext("Arellano-Bond AR(1)"), test = modelSummary[["m1"]]),
    list(label = gettext("Arellano-Bond AR(2)"), test = modelSummary[["m2"]])
  )

  undefinedTest <- FALSE

  for (entry in tests) {
    if (is.null(entry[["test"]]))
      next

    degreesOfFreedom <- entry[["test"]][["parameter"]]
    statistic        <- unname(entry[["test"]][["statistic"]])
    undefinedTest    <- undefinedTest || !is.finite(statistic)

    gmmTestsTable$addRows(list(
      test      = entry[["label"]],
      statistic = if (is.finite(statistic)) statistic else NA,
      df        = if (length(degreesOfFreedom) == 1) unname(degreesOfFreedom) else NA,
      pVal      = if (is.finite(statistic)) unname(entry[["test"]][["p.value"]]) else NA
    ))
  }

  if (exactlyIdentified)
    gmmTestsTable$addFootnote(gettext("The model is exactly identified, so there are no overidentifying restrictions to test."))

  if (undefinedTest)
    gmmTestsTable$addFootnote(gettext("A blank row could not be computed because its estimated variance is negative."))

  gmmTestsTable$addFootnote(gettext("Hansen J tests the overidentifying restrictions at the two-step estimator; it is robust to heteroskedasticity but loses power when there are many instruments. The Arellano-Bond tests assess serial correlation in the differenced residuals; AR(1) is expected to be significant, AR(2) is not."))

  return()
}

# Instrument proliferation invalidates the specification tests, so the number of
# instruments is reported next to the number of individuals.
.addGmmFitFootnotesPD <- function(table, fit, options) {
  nIndividuals <- length(fit[["W"]])
  nInstruments <- ncol(fit[["W"]][[1]])

  table$addFootnote(gettextf("The model uses %1$s instruments for %2$s individuals.", nInstruments, nIndividuals))

  if (nInstruments >= nIndividuals)
    table$addFootnote(gettext("There are at least as many instruments as individuals; consider collapsing the instrument matrix or reducing the lag range."),
                      symbol = gettext("Warning:"))

  if (options[["transformation"]] == "ld")
    table$addFootnote(gettext("System GMM assumes that the exogenous covariates and the changes of the instrumented variables are uncorrelated with the individual effects."))

  return()
}
