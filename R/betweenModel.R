#' @import jaspBase
#' @export
betweenModel <- function(jaspResults, dataset, options, analysis = "between") {
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
