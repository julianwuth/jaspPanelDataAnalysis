#' @import jaspBase
#' @export
panelDataAnalysis <- function(jaspResults, dataset, options) {

  ready <- .isReadyPD(dataset, options)

  if(ready) {
    .checkErrorsPD(dataset, options)
    options <- .rewriteOptionsPD(options, analysis = "within")
  }

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)

  return()
}


