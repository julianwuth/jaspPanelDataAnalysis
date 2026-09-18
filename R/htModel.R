#' @import jaspBase
#' @export
htModel <- function(jaspResults, dataset, options, analysis = "ht") {
  options <- .rewriteOptionsPD(options, analysis)
  ready   <- .isReadyPD(options)

  if (ready)
    .checkErrorsPD(dataset, options)

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)
  .htIdentificationPD(jaspResults, options, ready)

  if (options[["estimates"]])
    .coefficientsTablePD(jaspResults, dataset, options, ready,
                         robust = options[["robustEstimates"]],
                         deps   = c("estimates", "robustEstimates"))

  if (options[["plot"]])
    .createPlmPlot(jaspResults, dataset, options, ready)

  return()
}

# Identification requires at least as many exogenous time-varying regressors as
# there are endogenous time-invariant ones. plm reports this as an opaque
# estimation failure, so it is checked up front.
.htIdentificationPD <- function(jaspResults, options, ready) {
  if (!ready)
    return()

  nInstruments <- length(options[["timeVaryingExogenous"]])
  nEndogenous  <- length(options[["timeInvariantEndogenous"]])

  if (nInstruments >= nEndogenous)
    return()

  jaspResults[["modelSummaryTable"]]$setError(
    gettextf("The model is not identified: %1$s exogenous time-varying regressors are available to instrument %2$s endogenous time-invariant regressors.",
             nInstruments, nEndogenous)
  )

  return()
}
