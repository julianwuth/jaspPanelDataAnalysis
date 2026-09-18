#' @import jaspBase
#' @export
firstDifferenceModel <- function(jaspResults, dataset, options, analysis = "fd") {
  options <- .rewriteOptionsPD(options, analysis)
  ready   <- .isReadyPD(options)

  if (ready)
    .checkErrorsPD(dataset, options)

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)

  if (options[["estimates"]])
    .coefficientsTablePD(jaspResults, dataset, options, ready,
                         robust = options[["robustEstimates"]],
                         deps   = c("estimates", "robustEstimates"))

  if (options[["plot"]])
    .createPlmPlot(jaspResults, dataset, options, ready)

  return()
}
