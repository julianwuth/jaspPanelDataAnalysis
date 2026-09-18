#' @import jaspBase
#' @export
gmmModel <- function(jaspResults, dataset, options, analysis = "gmm") {
  options <- .rewriteOptionsPD(options, analysis)
  ready   <- .isReadyPD(options)

  if (ready)
    .checkErrorsPD(dataset, options)

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready, deps = "robustSE")
  .gmmInstrumentFootnotePD(jaspResults, options, ready)

  .coefficientsTablePD(jaspResults, dataset, options, ready, deps = "robustSE")

  .gmmSpecificationTestsTablePD(jaspResults, options, ready)

  if (options[["plot"]])
    .createPlmPlot(jaspResults, dataset, options, ready)

  return()
}

.gmmSpecificationTestsTablePD <- function(jaspResults, options, ready) {
  if (!is.null(jaspResults[["gmmTestsTable"]]))
    return()

  gmmTestsTable <- createJaspTable(title = gettext("Specification Tests"))
  gmmTestsTable$position <- 3
  gmmTestsTable$dependOn(c(.pdModelDeps, "robustSE"))
  gmmTestsTable$addCitation(.pdCitation)
  gmmTestsTable$addCitation("Arellano, M., & Bond, S. (1991). Some tests of specification for panel data: Monte Carlo evidence and an application to employment equations. The Review of Economic Studies, 58(2), 277-297.")

  gmmTestsTable$addColumnInfo(name = "test",      title = gettext("Test"),      type = "string")
  gmmTestsTable$addColumnInfo(name = "statistic", title = gettext("Statistic"), type = "number")
  gmmTestsTable$addColumnInfo(name = "df",        title = gettext("df"),        type = "integer")
  gmmTestsTable$addColumnInfo(name = "pVal",      title = gettext("p"),         type = "pvalue")
  gmmTestsTable$showSpecifiedColumnsOnly <- TRUE

  jaspResults[["gmmTestsTable"]] <- gmmTestsTable

  if (!ready)
    return()

  fit <- jaspResults[["modelFit"]]$object

  if (isTryError(fit))
    return()

  modelSummary <- try(summary(fit, robust = options[["robustSE"]]), silent = TRUE)

  if (isTryError(modelSummary)) {
    gmmTestsTable$setError(gettextf("The specification tests could not be performed: %s", .cleanErrorPD(modelSummary)))
    return()
  }

  tests <- list(
    list(label = gettext("Sargan"), test = modelSummary[["sargan"]]),
    list(label = gettext("Arellano-Bond AR(1)"), test = modelSummary[["m1"]]),
    list(label = gettext("Arellano-Bond AR(2)"), test = modelSummary[["m2"]])
  )

  for (entry in tests) {
    if (is.null(entry[["test"]]))
      next

    degreesOfFreedom <- entry[["test"]][["parameter"]]

    gmmTestsTable$addRows(list(
      test      = entry[["label"]],
      statistic = unname(entry[["test"]][["statistic"]]),
      df        = if (length(degreesOfFreedom) == 1) unname(degreesOfFreedom) else NA,
      pVal      = unname(entry[["test"]][["p.value"]])
    ))
  }

  gmmTestsTable$addFootnote(gettext("The Sargan test assesses the validity of the overidentifying restrictions. The Arellano-Bond tests assess serial correlation in the differenced residuals; AR(1) is expected to be significant, AR(2) is not."))

  return()
}

# Instrument proliferation invalidates the specification tests, so the number of
# instruments is reported next to the number of individuals.
.gmmInstrumentFootnotePD <- function(jaspResults, options, ready) {
  if (!ready)
    return()

  modelSummaryTable <- jaspResults[["modelSummaryTable"]]

  if (is.null(modelSummaryTable) || modelSummaryTable$getError())
    return()

  fit <- jaspResults[["modelFit"]]$object

  if (isTryError(fit))
    return()

  nIndividuals <- length(fit[["W"]])
  nInstruments <- ncol(fit[["W"]][[1]])

  modelSummaryTable$addFootnote(
    gettextf("The model uses %1$s instruments for %2$s individuals.", nInstruments, nIndividuals)
  )

  if (nInstruments >= nIndividuals)
    modelSummaryTable$addFootnote(
      gettext("There are at least as many instruments as individuals; consider collapsing the instrument matrix or reducing the lag range."),
      symbol = gettext("Warning:")
    )

  return()
}
