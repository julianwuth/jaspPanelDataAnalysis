#' @import jaspBase
#' @export
panelDataAnalysis <- function(jaspResults, dataset, options) {
  saveRDS(options, "/Users/julian/Documents/Jasp files/options.rds")

  ready <- .isReadyPD(dataset, options)

  if(ready)
    .checkErrorsPD(dataset, options)

  .modelSummaryTablePD(jaspResults, dataset, options, ready)

  return()
}


