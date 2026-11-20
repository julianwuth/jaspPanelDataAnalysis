#' @import jaspBase
#' @export
poolingModel <- function(jaspResults, dataset, options, analysis = "pooling") {
  options <- .rewriteOptionsPD(options, analysis)
  ready   <- .isReadyPD(options)

  if (ready)
    .checkErrorsPD(dataset, options)

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready, deps = "robustEstimates")

  if (options[["estimates"]])
    .coefficientsTablePD(jaspResults, dataset, options, ready, deps = c("estimates", "robustEstimates"))

  if (options[["plot"]])
    .createPlmPlot(jaspResults, dataset, options, ready)

  .lmTestPD(jaspResults, options, ready)

  return()
}
