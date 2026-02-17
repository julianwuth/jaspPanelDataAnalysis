#' @import jaspBase
#' @export
.createPLMPlot <- function(jaspResults, dataset, options){

  # Checks if the plot already exists
  if (!is.null(jaspResults[["plot"]])) return()

  image = createJaspPlot(title = "Panel Plot", width = 500, height = 400)

  image$dependOn(options = plot)

  image$plotFunction = function() {

    plm::plot.plm(x = model)

  }
  jaspResults[["modelPlot"]] <- image
}
