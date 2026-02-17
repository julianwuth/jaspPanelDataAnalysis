#' @import jaspBase
#' @export
fixedModel <- function(jaspResults, dataset, options, analysis = "within") {
  ready <- .isReadyPD(dataset, options)

  if(ready) {
    .checkErrorsPD(dataset, options)
    options <- .rewriteOptionsPD(options, analysis)
  }

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)

  if(options$estimates)
    .coefficientsTablePD(jaspResults, dataset, options, ready)

  if(options$fixedEffects)
    .fixedEffTablePD(jaspResults, dataset, options, ready)

  return()
}
