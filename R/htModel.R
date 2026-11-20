#' @import jaspBase
#' @export
htModel <- function(jaspResults, dataset, options, analysis = "ht") {
  options <- .rewriteOptionsPD(options, analysis)
  ready   <- .isReadyPD(options)

  if (ready)
    .checkErrorsPD(dataset, options)

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)

  if (options[["estimates"]])
    .coefficientsTablePD(jaspResults, dataset, options, ready, deps = "estimates")

  if (options[["plot"]])
    .createPlmPlot(jaspResults, dataset, options, ready)

  return()
}

# The second formula part lists the exogenous regressors, the third the time-varying
# endogenous ones, which are instrumented by their deviations from individual means
# (Wooldridge, 2002, pp. 326-327, Eqs. 11.68, 11.70).
.htInstrumentPartsPD <- function(options) {
  exogenous  <- .variablesPD(options[["timeVaryingExogenous"]], options[["timeInvariantExogenous"]])
  endogenous <- .variablesPD(options[["timeVaryingEndogenous"]])
  parts      <- paste(exogenous, collapse = " + ")

  if (length(endogenous) > 0)
    parts <- c(parts, paste(endogenous, collapse = " + "))

  return(parts)
}

# the estimator is only defined for individual effects
.fitHausmanTaylorPD <- function(formula, plmDf, options) {
  nInstruments <- length(options[["timeVaryingExogenous"]])
  nEndogenous  <- length(options[["timeInvariantEndogenous"]])

  # plm's variance components need K1 >= J2 for every instrument set
  # (Wooldridge, 2002, p. 327)
  if (nInstruments < nEndogenous)
    stop(gettextf("This estimator requires at least as many exogenous time-varying regressors (%1$s) as endogenous time-invariant regressors (%2$s).",
                  nInstruments, nEndogenous), call. = FALSE)

  fit <- plm::plm(formula, data = plmDf, model = "random",
                  effect        = "individual",
                  random.method = "ht",
                  inst.method   = options[["htMethod"]])

  return(fit)
}

# plm decides from the data what is time-constant, so a misplaced variable would
# silently change which instruments are used
.checkHausmanTaylorVariablesPD <- function(dataset, options) {
  id <- dataset[[options[["id"]]]]

  variesWithin <- function(variable) {
    values <- as.numeric(as.factor(dataset[[variable]]))

    return(any(tapply(values, id, function(x) length(unique(x[!is.na(x)])) > 1), na.rm = TRUE))
  }

  timeVarying   <- .variablesPD(options[["timeVaryingExogenous"]], options[["timeVaryingEndogenous"]])
  timeInvariant <- .variablesPD(options[["timeInvariantExogenous"]], options[["timeInvariantEndogenous"]])

  for (variable in timeVarying)
    if (!variesWithin(variable))
      .quitAnalysis(gettextf("%s is listed as time-varying but does not vary within any individual.",
                             jaspBase::decodeColNames(variable)))

  for (variable in timeInvariant)
    if (variesWithin(variable))
      .quitAnalysis(gettextf("%s is listed as time-invariant but varies within individuals.",
                             jaspBase::decodeColNames(variable)))

  return()
}

.addHtFitFootnotesPD <- function(table, fit) {
  # ercomp() computes one transformation parameter from the longest time series
  if (!plm::pdim(fit)[["balanced"]])
    table$addFootnote(gettext("On an unbalanced panel plm uses one quasi-demeaning parameter for all individuals, so the estimates and standard errors are approximate."),
                      symbol = gettext("Warning:"))

  return()
}
