#' @import jaspBase
#' @export
pgglsModel <- function(jaspResults, dataset, options, analysis = "pggls") {
  options <- .rewriteOptionsPD(options, analysis)
  ready   <- .isReadyPD(options)

  if (ready)
    .checkErrorsPD(dataset, options)

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)
  .pgglsDimensionFootnotePD(jaspResults, options, ready)

  .coefficientsTablePD(jaspResults, dataset, options, ready)

  if (options[["plot"]])
    .createPlmPlot(jaspResults, dataset, options, ready)

  return()
}

# The unrestricted error covariance matrix is only estimable when the number of
# individuals exceeds the number of periods.
.pgglsDimensionFootnotePD <- function(jaspResults, options, ready) {
  if (!ready)
    return()

  modelSummaryTable <- jaspResults[["modelSummaryTable"]]

  if (is.null(modelSummaryTable) || modelSummaryTable$getError())
    return()

  fit <- jaspResults[["modelFit"]]$object

  if (isTryError(fit))
    return()

  panelDimensions <- try(plm::pdim(fit), silent = TRUE)

  if (isTryError(panelDimensions))
    return()

  nIndividuals <- panelDimensions[["nT"]][["n"]]
  nPeriods     <- panelDimensions[["nT"]][["T"]]

  if (nIndividuals <= nPeriods)
    modelSummaryTable$addFootnote(
      gettextf("The general FGLS estimator requires more individuals (%1$s) than time periods (%2$s).",
               nIndividuals, nPeriods),
      symbol = gettext("Warning:")
    )

  return()
}
