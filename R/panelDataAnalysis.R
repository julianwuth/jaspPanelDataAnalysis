#' @import jaspBase
#' @export
panelDataAnalysis <- function(jaspResults, dataset, options) {
  saveRDS(options, "/Users/julian/Documents/Jasp files/options.rds")

  ready <- .isReadyPD(dataset, options)

  if(ready)
    .checkErrorsPD(dataset, options)

  .fitModelPD(jaspResults, dataset, options, ready)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)

  return()
}


