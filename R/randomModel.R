#' @import jaspBase
#' @export
randomModel <- function(jaspResults, dataset, options, analysis = "random") {

  ready <- .isReadyPD(dataset, options)

  if(ready) {
    .checkErrorsPD(dataset, options)
    options <- .rewriteOptionsPD(options, analysis)
  }

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)

  if(options$estimates)
    .coefficientsTablePD(jaspResults, dataset, options, ready)

  if(options$randomEffects)
    .randEffTablePD(jaspResults, dataset, options, ready)

  if(options$plot)
    .createPlmPlot(jaspResults, dataset, options, ready)

  return()
}
