#' @import jaspBase
#' @export
panelDataAnalysis <- function(jaspResults, dataset, options, analysis = "random") {

  ready <- .isReadyPD(dataset, options)

  if(ready) {
    .checkErrorsPD(dataset, options)
    options <- .rewriteOptionsPD(options, analysis)
  }

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)

  if(options$estimates)
    .coefficientsTablePD(jaspResults, dataset, options, ready)

  if(analysis == "within")
    .fixedEffTablePD(jaspResults, dataset, options, ready)

  if(analysis == "random")
    .randEffTablePD(jaspResults, dataset, options, ready)

  return()
}


