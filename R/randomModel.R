#' @import jaspBase
#' @export
randomModel <- function(jaspResults, dataset, options, analysis = "random") {
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

  if (options[["randomEffects"]])
    .randEffTablePD(jaspResults, dataset, options, ready)

  if (options[["plot"]])
    .createPlmPlot(jaspResults, dataset, options, ready)

  .hausmanTestPD(jaspResults, dataset, options, ready)

  return()
}
